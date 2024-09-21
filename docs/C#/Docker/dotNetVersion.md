### question
For a Dockerfile to use .NET 8 is this the correct FROM statement:
FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS base
     WORKDIR /app

### answer
To use .NET 8 in your Dockerfile, you should update the `FROM` statement to reference the correct .NET 8 image. Here's the updated `FROM` statement:

```dockerfile
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS base
WORKDIR /app
```

This specifies the base image for ASP.NET Core runtime for .NET 8².

Source: Conversation with Copilot, 9/21/2024
(1) Dockerize .NET 8 API: Easy Guide - Alex Taveras-Crespo. https://blog.alextaverascrespo.com/containerize-your-net-8-api-easily-with-docker-a-how-to-guide.
(2) Containerize an app with Docker tutorial - .NET | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/core/docker/build-container.
(3) Dockerfile reference | Docker Docs. https://docs.docker.com/reference/dockerfile/.
(4) Run an ASP.NET Core app in Docker containers | Microsoft Learn. https://learn.microsoft.com/en-us/aspnet/core/host-and-deploy/docker/building-net-docker-images?view=aspnetcore-8.0.
(5) Not able to run Docker image for .net 8 #5897 - GitHub. https://github.com/dotnet/dotnet-docker/issues/5897.