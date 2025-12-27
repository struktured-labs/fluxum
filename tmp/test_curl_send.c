#include <stdio.h>
#include <curl/curl.h>
#include <string.h>

int main() {
    CURL *curl = curl_easy_init();
    if (!curl) {
        fprintf(stderr, "curl_easy_init failed\n");
        return 1;
    }

    curl_easy_setopt(curl, CURLOPT_URL, "https://ws.kraken.com/");
    curl_easy_setopt(curl, CURLOPT_CONNECT_ONLY, 1L);
    curl_easy_setopt(curl, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1);
    curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
        fprintf(stderr, "curl_easy_perform failed: %s\n", curl_easy_strerror(res));
        curl_easy_cleanup(curl);
        return 1;
    }

    printf("\n=== Connection established, trying to send... ===\n");

    const char *request = "GET / HTTP/1.1\r\nHost: ws.kraken.com\r\n\r\n";
    size_t sent = 0;
    res = curl_easy_send(curl, request, strlen(request), &sent);
    if (res != CURLE_OK) {
        fprintf(stderr, "curl_easy_send failed: %s (code %d)\n",
                curl_easy_strerror(res), res);
        curl_easy_cleanup(curl);
        return 1;
    }

    printf("Sent %zu bytes\n", sent);

    char buf[1024];
    size_t received = 0;
    res = curl_easy_recv(curl, buf, sizeof(buf) - 1, &received);
    if (res != CURLE_OK) {
        fprintf(stderr, "curl_easy_recv failed: %s (code %d)\n",
                curl_easy_strerror(res), res);
        curl_easy_cleanup(curl);
        return 1;
    }

    buf[received] = '\0';
    printf("Received %zu bytes:\n%s\n", received, buf);

    curl_easy_cleanup(curl);
    return 0;
}
