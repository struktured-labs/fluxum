#include <curl/curl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>

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

/* curl_easy_send wrapper with retry on CURLE_AGAIN and timeout */
CAMLprim value caml_curl_easy_send(value v_conn, value v_buf, value v_offset, value v_len)
{
    CAMLparam4(v_conn, v_buf, v_offset, v_len);
    CAMLlocal1(v_err_msg);
    Connection *connection = Connection_val(v_conn);
    CURL *conn = connection->handle;

    const char *buf = (const char *)Bytes_val(v_buf) + Int_val(v_offset);
    size_t len = Int_val(v_len);
    size_t sent = 0;

    /* Retry loop for CURLE_AGAIN (non-blocking socket) */
    /* 200 retries * 10ms = 2 seconds max, ensuring pings complete in time */
    int max_retries = 200;
    for (int retry = 0; retry < max_retries; retry++) {
        CURLcode result = curl_easy_send(conn, buf, len, &sent);

        if (result == CURLE_OK) {
            CAMLreturn(Val_int(sent));
        }

        if (result == CURLE_AGAIN) {
            /* Socket not ready, wait a bit and retry */
            struct timespec ts = {0, 10000000};  /* 10ms */
            nanosleep(&ts, NULL);
            continue;
        }

        /* Other error */
        char err_buf[256];
        snprintf(err_buf, sizeof(err_buf), "curl_easy_send failed: %s (code %d)",
                 curl_easy_strerror(result), result);
        caml_failwith(err_buf);
    }

    /* Timeout after max retries */
    caml_failwith("curl_easy_send timeout: socket not ready after 2 seconds");
    CAMLreturn(Val_int(0));  /* unreachable */
}

/* curl_easy_recv wrapper with retry on CURLE_AGAIN */
CAMLprim value caml_curl_easy_recv(value v_conn, value v_buf, value v_offset, value v_len)
{
    CAMLparam4(v_conn, v_buf, v_offset, v_len);
    CAMLlocal1(v_err_msg);
    Connection *connection = Connection_val(v_conn);
    CURL *conn = connection->handle;

    char *buf = (char *)Bytes_val(v_buf) + Int_val(v_offset);
    size_t len = Int_val(v_len);
    size_t received = 0;

    /* Retry loop for CURLE_AGAIN (non-blocking socket) */
    int max_retries = 500;  /* 5 seconds total */
    for (int retry = 0; retry < max_retries; retry++) {
        CURLcode result = curl_easy_recv(conn, buf, len, &received);

        if (result == CURLE_OK) {
            CAMLreturn(Val_int(received));
        }

        if (result == CURLE_AGAIN) {
            /* Socket not ready, wait a bit and retry */
            struct timespec ts = {0, 10000000};  /* 10ms */
            nanosleep(&ts, NULL);
            continue;
        }

        /* Other error */
        char err_buf[256];
        snprintf(err_buf, sizeof(err_buf), "curl_easy_recv failed: %s (code %d)",
                 curl_easy_strerror(result), result);
        caml_failwith(err_buf);
    }

    /* Timeout after max retries */
    caml_failwith("curl_easy_recv timeout: socket not ready");
    CAMLreturn(Val_int(0));  /* unreachable */
}
