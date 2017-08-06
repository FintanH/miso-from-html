# miso-from-html

A command line tool for taking a HTML file and outputting Haskell code compatible for Miso.

Example usage:
`stack exec miso-from-html -- --inputFile index.html --indent 2 --outputFile View.hs`

#### Input: index.html
``` html
<body>
  <div class="hello">
    <p>Hello, World!</p>
  </div>
</body>
```

#### Output: View.hs
``` haskell
fn1 = body_
  []
  [ div_
    [ class_ "hello" ]
    [ p_
        []
        [ text "Hello, World!" ]
    ]
  ]
```
