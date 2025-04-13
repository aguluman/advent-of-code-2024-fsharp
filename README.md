[Advent of Code 2024](https://adventofcode.com/2024) in F# on Windows OS

## To validate the functions with the Test-Cases

```
$ dotnet test
```
## How to run the input datasets.

```
$ type "C:\***\******\****\input.txt" | dotnet run
```

## For Faster compilation time, 
```
$ dotnet build -c Release
```

```
$ cd bin\Release\netX.X
```

```
type "C:\***\******\****\input.txt" | .\day**.exe
```

### Note
The parse function for each challenge question’s input file was designed to handle the ```CRLF``` end of line sequence.