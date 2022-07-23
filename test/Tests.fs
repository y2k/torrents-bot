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
- ONE, Murata Yusuke / Ван, Мурата Юсукэ - Ванпанчме… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTQwNDk5Ng==">[ADD]</a>
- One Punch Man / Ванпанчмен [Art] [2020] [JPG] <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTkzNDk5Mw==">[ADD]</a>
- One-Punch Man: A Hero Nobody Knows - Deluxe Editio… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg1NzY4MQ==">[ADD]</a>
- (German Hip-Hop) MC Bomber - Official Discography… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTUxNDAwNg==">[ADD]</a>
- Ванпанчмен (ТВ-2) / One Punch Man 2 [TV] [1-4 из 1… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg3MzE2OA==">[ADD]</a>
- One Finger Death Punch 2 [L] [RUS + ENG / ENG] (20… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTcxOTE1OQ==">[ADD]</a>
- [Nintendo Switch] One Finger Death Punch 2 [NSZ][E… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTg2MjEyOQ==">[ADD]</a>
- (Ongoing) One, Murata Yuusuke / Ван, Мурата Июсуке… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NDk1NTUxNw==">[ADD]</a>
- Человек-тапок / One-Punch Man / Серии: 01-04 [2019… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTgwMTQ4NA==">[ADD]</a>
- Ванпанчмен / One-Punch Man (ТВ-2) (Сакурай Тикара)… <a href="https://t.me/$ME$?start=c2l0ZS5jb20vZm9ydW0vZGwucGhwP3Q9NTc2ODYwOA==">[ADD]</a>"""

    let app = runTestApplication ()
    app.writeToBot "/search site.com one punch"

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
