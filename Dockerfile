FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

WORKDIR /app
COPY torrents-bot.sln .
COPY bot/*.fsproj bot/
COPY test/*.fsproj test/

RUN dotnet restore

COPY bot/*.fs bot/
COPY bot/common/*.fs bot/common/
COPY test/*.fs test/
COPY test/common/*.fs test/common/
COPY test/common/__test_data/*.zip test/common/__test_data

ARG TEST_PWD
ENV TEST_PWD=$TEST_PWD

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

WORKDIR /app
COPY --from=0 /app/bot/bin/Release/net6.0/linux-x64/publish .

ENTRYPOINT ["dotnet", "bot.dll"]