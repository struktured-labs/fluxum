#include <curl/curl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <string.h>
#include <stdlib.h>

/* ocurl's Connection structure (from curl-helper.c) */
typedef struct Connection {
    CURL *handle;
    /* ... other fields omitted ... */
} Connection;

#define Connection_val(v) (*(Connection**)Data_custom_val(v))

/* Set socket to non-blocking mode */
CAMLprim value caml_curl_set_nonblocking(value v_conn)
{
    CAMLparam1(v_conn);
    Connection *connection = Connection_val(v_conn);
    CURL *conn = connection->handle;

    /* Get the active socket file descriptor */
    curl_socket_t sockfd;
    CURLcode result = curl_easy_getinfo(conn, CURLINFO_ACTIVESOCKET, &sockfd);
    if (result != CURLE_OK) {
        char err_buf[256];
        snprintf(err_buf, sizeof(err_buf), "curl_easy_getinfo ACTIVESOCKET failed: %s",
                 curl_easy_strerror(result));
        caml_failwith(err_buf);
    }

    /* Set socket to non-blocking mode */
    int flags = fcntl(sockfd, F_GETFL, 0);
    if (flags == -1) {
        caml_failwith("fcntl F_GETFL failed");
    }
    if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) == -1) {
        caml_failwith("fcntl F_SETFL O_NONBLOCK failed");
    }

    CAMLreturn(Val_unit);
}

/* curl_easy_send wrapper - releases OCaml runtime lock during blocking wait.
   Uses poll() instead of busy-wait nanosleep for efficiency. */
CAMLprim value caml_curl_easy_send(value v_conn, value v_buf, value v_offset, value v_len)
{
    CAMLparam4(v_conn, v_buf, v_offset, v_len);

    /* Extract C-level data while holding the runtime lock */
    Connection *connection = Connection_val(v_conn);
    CURL *conn = connection->handle;
    size_t len = Int_val(v_len);
    int offset = Int_val(v_offset);

    /* Get socket fd for poll() */
    curl_socket_t sockfd;
    CURLcode info_result = curl_easy_getinfo(conn, CURLINFO_ACTIVESOCKET, &sockfd);
    if (info_result != CURLE_OK || sockfd == CURL_SOCKET_BAD) {
        caml_failwith("curl_easy_send: cannot get active socket");
    }

    /* Copy data to C-heap buffer (safe when OCaml GC runs in other threads) */
    char *tmp_buf = (char *)malloc(len);
    if (!tmp_buf) {
        caml_failwith("curl_easy_send: malloc failed");
    }
    memcpy(tmp_buf, (const char *)Bytes_val(v_buf) + offset, len);

    /* Release OCaml runtime lock so other threads (especially the Async
       scheduler processing cohttp HTTP responses) can run */
    caml_enter_blocking_section();

    size_t sent = 0;
    CURLcode last_result = CURLE_AGAIN;
    int max_retries = 200;  /* 200 * 10ms = 2 seconds max */

    for (int retry = 0; retry < max_retries; retry++) {
        last_result = curl_easy_send(conn, tmp_buf, len, &sent);

        if (last_result == CURLE_OK) break;
        if (last_result != CURLE_AGAIN) break;

        /* Wait for socket writability using poll() - efficient, no busy-wait */
        struct pollfd pfd = { .fd = sockfd, .events = POLLOUT, .revents = 0 };
        poll(&pfd, 1, 10);  /* 10ms timeout */
    }

    /* Re-acquire OCaml runtime lock */
    caml_leave_blocking_section();

    free(tmp_buf);

    if (last_result == CURLE_OK) {
        CAMLreturn(Val_int(sent));
    }

    if (last_result == CURLE_AGAIN) {
        caml_failwith("curl_easy_send timeout: socket not ready after 2 seconds");
    }

    char err_buf[256];
    snprintf(err_buf, sizeof(err_buf), "curl_easy_send failed: %s (code %d)",
             curl_easy_strerror(last_result), last_result);
    caml_failwith(err_buf);
    CAMLreturn(Val_int(0));  /* unreachable */
}

/* curl_easy_recv wrapper - releases OCaml runtime lock during blocking wait.
   Uses a temporary C-heap buffer and poll() for efficient non-blocking I/O.
   This is critical: without releasing the runtime lock, WebSocket recv threads
   block the Async scheduler, causing cohttp REST API calls to take 35-50s. */
CAMLprim value caml_curl_easy_recv(value v_conn, value v_buf, value v_offset, value v_len)
{
    CAMLparam4(v_conn, v_buf, v_offset, v_len);

    /* Extract C-level data while holding the runtime lock.
       The CURL handle lives on the C heap (allocated by curl_easy_init),
       so it remains valid after we release the OCaml runtime lock. */
    Connection *connection = Connection_val(v_conn);
    CURL *conn = connection->handle;
    size_t len = Int_val(v_len);
    int offset = Int_val(v_offset);

    /* Get socket fd for poll() */
    curl_socket_t sockfd;
    CURLcode info_result = curl_easy_getinfo(conn, CURLINFO_ACTIVESOCKET, &sockfd);
    if (info_result != CURLE_OK || sockfd == CURL_SOCKET_BAD) {
        caml_failwith("curl_easy_recv: cannot get active socket");
    }

    /* Allocate temp buffer on C heap - we can't write to the OCaml buffer
       while the runtime lock is released (GC could move it) */
    char *tmp_buf = (char *)malloc(len);
    if (!tmp_buf) {
        caml_failwith("curl_easy_recv: malloc failed");
    }

    /* Release OCaml runtime lock so the Async scheduler and other threads
       can run while we wait for data */
    caml_enter_blocking_section();

    size_t received = 0;
    CURLcode last_result = CURLE_AGAIN;
    int max_retries = 3000;  /* 3000 * 10ms = 30 seconds max */

    for (int retry = 0; retry < max_retries; retry++) {
        last_result = curl_easy_recv(conn, tmp_buf, len, &received);

        if (last_result == CURLE_OK) break;
        if (last_result != CURLE_AGAIN) break;

        /* Wait for socket readability using poll() - efficient, no busy-wait.
           poll() truly sleeps the thread, releasing CPU and allowing
           the OS scheduler to run other threads. */
        struct pollfd pfd = { .fd = sockfd, .events = POLLIN, .revents = 0 };
        poll(&pfd, 1, 10);  /* 10ms timeout */
    }

    /* Re-acquire OCaml runtime lock */
    caml_leave_blocking_section();

    if (last_result == CURLE_OK && received > 0) {
        /* Copy from temp buffer to OCaml buffer (safe: we hold the lock) */
        memcpy((char *)Bytes_val(v_buf) + offset, tmp_buf, received);
        free(tmp_buf);
        CAMLreturn(Val_int(received));
    }

    free(tmp_buf);

    if (last_result == CURLE_AGAIN) {
        caml_failwith("curl_easy_recv timeout: socket not ready");
    }

    char err_buf[256];
    snprintf(err_buf, sizeof(err_buf), "curl_easy_recv failed: %s (code %d)",
             curl_easy_strerror(last_result), last_result);
    caml_failwith(err_buf);
    CAMLreturn(Val_int(0));  /* unreachable */
}
