    let Genre_ = <Haskell : {} | Cpp : {} | JavaScript : {} | Rust : {} | Go : {}>
in  let genreHandler = {
    Haskell = \(_ : {}) -> "Haskell"
  , Cpp = \(_ : {}) -> "C++"
  , JavaScript = \(_ : {}) -> "JavaScript"
  , Rust = \(_ : {}) -> "Rust"
  , Go = \(_ : {}) -> "Go"
  }
in  {  
    Genre = Genre_ 
  , genreToText = \(g : Genre_) -> merge genreHandler g
}
