
(use srfi-1)

(declare (uses json))
(declare (uses new-customer-service))
(declare (uses sql))

(with-parsed-json-object
  (string-append
    "{"
    "  \"first-name\": \"Mathieu\", "
    "  \"last-name\": \"Desrochers\", "
    "  \"addresses\": "
    "  ["
    "    {"
    "      \"address\": \"123 Sunny Street\", "
    "      \"city\": \"Sun City\", "
    "      \"state\": \"Florida\""
    "    }, "
    "    {"
    "      \"address\": \"456 Cloudy Street\", "
    "      \"city\": \"London\", "
    "      \"state\": \"UK\""
    "    }"
    "  ]"
    "}")
  (lambda (json-object-request)
    (let ((new-customer-request (parse-new-customer-request json-object-request)))
      (with-sql-connection "customers.db"
        (lambda (sql-connection)
          (let ((new-customer-response (new-customer-service sql-connection new-customer-request)))
            (with-new-json-object
              (lambda (json-object-response)
                (format-new-customer-response new-customer-response json-object-response)
                (display (json-object->string json-object-response))))))))))
