module Tests

open Xunit
open TestFramework
open Swensen.Unquote

[<Fact>]
let ``test my_rss_link`` () =
    let app = runTestApplication ()
    app.writeToBot "/my_rss_link"

    assertWithRetry (fun _ ->
        let actual = app.readFromBot ()

        let expected =
            [ "You RSS link: http://localhost:8080/rss/HQ_sqXn-2x2OpeBxq7Ed9cB3z5ilKPyWSASCiNLwbSc~" ]

        test <@ expected = actual @>)

[<Fact>]
let ``test search`` () =
    let expected =
        """Search result:
ONE, Murata Yusuke / Ван, Мурата Юсукэ - Ванпанчме… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTQwNDk5Ng==">[DOWNLOAD]</a>
One Punch Man / Ванпанчмен [Art] [2020] [JPG] <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTkzNDk5Mw==">[DOWNLOAD]</a>
One-Punch Man: A Hero Nobody Knows - Deluxe Editio… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg1NzY4MQ==">[DOWNLOAD]</a>
(German Hip-Hop) MC Bomber - Official Discography… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTUxNDAwNg==">[DOWNLOAD]</a>
Ванпанчмен (ТВ-2) / One Punch Man 2 [TV] [1-4 из 1… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg3MzE2OA==">[DOWNLOAD]</a>
One Finger Death Punch 2 [L] [RUS + ENG / ENG] (20… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTcxOTE1OQ==">[DOWNLOAD]</a>
[Nintendo Switch] One Finger Death Punch 2 [NSZ][E… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg2MjEyOQ==">[DOWNLOAD]</a>
(Ongoing) One, Murata Yuusuke / Ван, Мурата Июсуке… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NDk1NTUxNw==">[DOWNLOAD]</a>
Человек-тапок / One-Punch Man / Серии: 01-04 [2019… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTgwMTQ4NA==">[DOWNLOAD]</a>
Ванпанчмен / One-Punch Man (ТВ-2) (Сакурай Тикара)… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTc2ODYwOA==">[DOWNLOAD]</a>"""

    let app = runTestApplication ()
    app.writeToBot "/search site.com one punch"

    assertWithRetry (fun _ ->
        let actual = app.readFromBot ()
        let expected = [ expected; "Search…" ]

        test <@ expected = actual @>)

[<Fact>]
let ``test search anime`` () =
    let expected =
        """Search result:
[Gumroad / Giulia Marchetti] 3D Character Creation… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI5MjM2">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Downtempo) [WEB]… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4NzYz">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Downtempo) [WEB]… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4NzYy">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Downtempo) [WEB]… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4NzYx">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Downtempo) [WEB]… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4NzYw">[DOWNLOAD]</a>
По волчьим законам / Animal Kingdom / Сезон: 6 / С… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4NTcx">[DOWNLOAD]</a>
[Nintendo Switch] Bit Orchard Animal Valley + DLC… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjA2Mjgw">[DOWNLOAD]</a>
По волчьим законам / Animal Kingdom / Сезон: 6 / С… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4MjM1">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Ambient) [WEB] G… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4MjI5">[DOWNLOAD]</a>
[TR24][OF][AF] (Score, Anime OST, Ambient) [WEB] G… <a href="https://t.me/$ME$?start=cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD02MjI4MjI4">[DOWNLOAD]</a>"""

    let app = runTestApplication ()
    app.writeToBot "/search rutracker.org anime"

    assertWithRetry (fun _ ->
        let actual = app.readFromBot ()
        let expected = [ expected; "Search…" ]

        test <@ expected = actual @>)

[<Fact>]
let ``test start`` () =
    let expected =
        """<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
   <item>
      <title>FIXME</title>
      <link>http://localhost:8080/t/7LrU79d8OYfhAg-7LLyviJuDOVc0DFLMdZ-X9pDSACM.</link>
    </item>
  </channel>
</rss>"""

    let app = runTestApplication ()
    app.writeToBot "/start cnV0cmFja2VyLm9yZy9mb3J1bS9kbC5waHA_dD01NDA0OTk2"

    assertWithRetry (fun _ ->
        let actual =
            app.downloadString "http://localhost:8080/rss/HQ_sqXn-2x2OpeBxq7Ed9cB3z5ilKPyWSASCiNLwbSc~"

        test <@ expected = actual @>)
