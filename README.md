# nim-monkey


[Go言語でつくるインタプリタ](https://www.oreilly.co.jp/books/9784873118222/)をNimで写経

- [x] １章
- [x] ２章
- [ ] ３章
- [ ] ４章
## 参考

* https://github.com/mrsekut/monkey-nim
  * これを写経する　

|項目|内容|使い分け|
|---|---|---|
|var|ミュータブルな変数|変数の値を変更する必要がある時|
|let|イミュータブルな変数|変数の値を変更しない時。基本的にこれ|
|const|イミュータブルな変数|コンパイル時に値が確定している時|


|項目|内容|使い分け|
|---|---|---|
|type 〇〇 = enum|列挙型|種類などを列挙したい時|
|type 〇〇 = object|スタックに格納される|宣言後にフィールドの値を変える必要がないとき|
|type 〇〇 = ref object|ヒープに格納される|宣言後にフィールドの値を変更する必要が出た場合|
|type 〇〇 = ptr object|GCでこれは破棄されない|GCのタイミングをコントロールしたい場合|
|type <br>  〇〇 = ref object<br>  NotNil〇〇 = 〇〇 not nil|宣言時にNilを許容しない||

|項目|内容|使い分け|
|---|---|---|
|proc(プロシージャ)|基本的な静的なファンクション|method、funcを使う必要がない時|
|method(メソッド)|構造体の継承後にオーバーライドできる|オブジェクト指向っぽいことをしたいとき|
|func(ファンクション)|副作用を許さない|副作用がない場合|


