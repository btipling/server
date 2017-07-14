# server

An HTTP server built with Haskell on Windows TCP sockets.

A work in progress and a project to learn and improve my Haskell knowledge.

## Progress

Currently outputs directory contents and file contents for a given path.

**Security note:** don't use this program in an untrusted environment or run the application as a restricted user as this program does not prevent the reading of files outside of the working directory.

```
stack build; stack exec server "C:\Users\swart\projects\haskell\server\"
```

Outputs:

```
𝒜 ☃ was visited by Request {httpRequestRaw = "GET /? HTTP/1.1\r\nAccept: text/html, application/xhtml+xml, image/jxr, */*\r\nAccept-Language: en-US,en;q=0.5\r\nUser-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36 Edge/15.15063\r\nAccept-Encoding: gzip, deflate\r\nHost: localhost:8080\r\nDNT: 1\r\nConnection: Keep-Alive\r\n\r\n", httpRequestContent = "", httpRequestMethod = 1, httpRequestHeaders = fromList [("Accept","text/html, application/xhtml+xml, image/jxr, */*"),("Accept-Encoding","gzip, deflate"),("Accept-Language","en-US,en;q=0.5"),("Connection","Keep-Alive"),("DNT","1"),("Host","localhost:8080"),("User-Agent","Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36 Edge/15.15063")], httpRequestQuery = fromList [("","")], httpRequestPathString = "/", httpRequestPathList = ["",""]}!
C:\Users\swart\projects\haskell\server\

 -> stack.yaml
 -> src
 -> Setup.hs
 -> server.cabal
 -> README.md
 -> LICENSE
 -> .stack-work
 -> .history
 -> .gitignore
 -> .git
 ```
