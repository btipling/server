# server

A simle HTTP server built with Haskell on Windows TCP sockets to browse files.

## Progress

* Currently outputs directory contents and file contents for a given path.
* Crashes on non-utf8 file contents.

**Security note:** don't use this program in an untrusted environment or run the application as a restricted user as this program does not prevent the reading of files outside of the working directory.

```
stack build; stack exec server "C:\Users\swart\projects\haskell\server\"
```

### Screenshots

![image](https://user-images.githubusercontent.com/249641/28255277-db8ec072-6a69-11e7-80de-3c437da6a98f.png)

![image](https://user-images.githubusercontent.com/249641/28255292-ffc01df6-6a69-11e7-9254-a477d4a9485d.png)
