# query GET

    Code
      cat(rawToChar(query("/get")$content))
    Output
      {
        "args": {},
        "headers": {
          "Host": "127.0.0.1:<port>",
          "Accept-Encoding": "<encodings>",
          "accept": "application/json",
          "content-type": "application/json",
          "user-agent": "R-hub client"
        },
        "origin": "127.0.0.1",
        "path": "/get",
        "url": "http://127.0.0.1:<port>/get"
      }

# query HTTP errors

    Code
      query("/rhub-error?msg=iamsosorryabouththat")
    Condition
      Error:
      ! iamsosorryabouththat
      Caused by error:
      ! Unauthorized (HTTP 401).

---

    Code
      query("/rhub-error2")
    Condition
      Error:
      ! Unauthorized (HTTP 401).

---

    Code
      query("/rhub-error3")
    Condition
      Error:
      ! Unauthorized (HTTP 401).

# query POST

    Code
      cat(rawToChar(query("/post", method = "POST", data = data)$content))
    Output
      {
        "args": {},
        "data": "{\"foo\":[\"bar\"],\"foobar\":[1,2,3]}",
        "files": {},
        "form": {},
        "headers": {
          "Host": "127.0.0.1:<port>",
          "Accept-Encoding": "<encodings>",
          "accept": "application/json",
          "content-type": "application/json",
          "user-agent": "R-hub client",
          "Content-Length": "32"
        },
        "json": {
          "foo": [
            "bar"
          ],
          "foobar": [
            1,
            2,
            3
          ]
        },
        "method": "post",
        "path": "/post",
        "origin": "127.0.0.1",
        "url": "http://127.0.0.1:<port>/post"
      }

# query, unknown verb

    Code
      query("/anything", method = "REPORT")
    Condition
      Error:
      ! Unexpected HTTP verb, internal rhub error
    Code
      query("/anything", method = "REPORT", sse = TRUE)
    Condition
      Error:
      ! Unexpected HTTP verb, internal rhub error

# query SSE

    Code
      query("/sse", sse = TRUE)$sse
    Output
      [[1]]
                        event                 message 
                          "1" "live long and prosper" 
      
      [[2]]
                        event                 message 
                          "2" "live long and prosper" 
      
      [[3]]
                        event                 message 
                          "3" "live long and prosper" 
      
      [[4]]
                        event                 message 
                          "4" "live long and prosper" 
      
      [[5]]
                        event                 message 
                          "5" "live long and prosper" 
      
    Code
      query("/sse", method = "POST", data = data, sse = TRUE)$sse
    Output
      [[1]]
                        event                 message 
                          "1" "live long and prosper" 
      
      [[2]]
                        event                 message 
                          "2" "live long and prosper" 
      
      [[3]]
                        event                 message 
                          "3" "live long and prosper" 
      
      [[4]]
                        event                 message 
                          "4" "live long and prosper" 
      
      [[5]]
                        event                 message 
                          "5" "live long and prosper" 
      

---

    Code
      resp <- query("/sse?progress=true&numevents=2", sse = TRUE)
    Message
      > This is `it`: 1
      > This is `it`: 2
      v Done.
    Code
      cat(rawToChar(resp$content))
    Output
      event: 1
      message: live long and prosper
      
      event: progress
      data: "This is {.code it}: 1"
      
      event: 2
      message: live long and prosper
      
      event: progress
      data: "This is {.code it}: 2"
      
      event: result
      data: "All is {.code good}."
      

---

    Code
      resp <- query("/sse?progress=true&numevents=2&error=true", sse = TRUE)
    Message
      > This is `it`: 1
      > This is `it`: 2
      x This is a `failure`.
    Condition
      Error:
      ! Aborting
    Code
      cat(rawToChar(resp$content))
    Output
      event: 1
      message: live long and prosper
      
      event: progress
      data: "This is {.code it}: 1"
      
      event: 2
      message: live long and prosper
      
      event: progress
      data: "This is {.code it}: 2"
      
      event: result
      data: "All is {.code good}."
      

