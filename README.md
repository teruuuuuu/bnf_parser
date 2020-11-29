## Bnf Parser

### Usage
#### check if parse success
```
import com.github.kmizu.scomb.Result
import jp.co.teruuu.bnf.BnfObject.BnfParser

val bnf_definition = List("<number> ::= (\"0\"|\"1\"|\"2\"|\"3\"|\"4\"|\"5\"|\"6\"|\"7\"|\"8\"|\"9\")",
  "<lower> ::= (\"a\"|\"b\"|\"c\"|\"d\"|\"e\"|\"f\"|\"g\"|\"h\"|\"i\"|\"j\"|\"k\"|\"l\"|\"m\"|\"n\"|\"o\"|\"p\"|\"q\"|\"r\"|\"s\"|\"t\"|\"u\"|\"v\"|\"w\"|\"x\"|\"y\"|\"z\")",
  "<upper> ::= (\"A\"|\"B\"|\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"H\"|\"I\"|\"J\"|\"K\"|\"L\"|\"M\"|\"N\"|\"O\"|\"P\"|\"Q\"|\"R\"|\"S\"|\"T\"|\"U\"|\"V\"|\"W\"|\"X\"|\"Y\"|\"Z\")",
  "<comma> ::= \".\"",
  "<char> ::= (<number>|<lower>|<upper>|<comma>)",
  "<host> ::= <char> +",
  "<dir> ::= (<char>|\"/\") +",
  "<key> ::= (<lower>|<upper>)+",
  "<value> ::= (<lower>|<upper>|<number>)+",
  "<param> ::= <key> \"=\" <value>",
  "<params> ::= <param> (\"&\"<param>)*",
  "<scheme> ::= \"https\"|\"http\"",
  "<uri> ::= <scheme> \"://\" <host> [\"/\" <dir> [\"?\" <params>]]",
  "<root> ::= <uri>"
)
BnfParser.apply(bnf_definition) match {
  case (_, Some(parser)) => {
    println(s"=> ${parser.parse("https://github.com/user/project?abc=def&ghi=jkl").isInstanceOf[Result.Success[_]]}")
  }
}
```
```
=> true
```

#### convert
```
import jp.co.teruuu.bnf.Converter

val bnf_definition = List("<number> ::= (\"0\"|\"1\"|\"2\"|\"3\"|\"4\"|\"5\"|\"6\"|\"7\"|\"8\"|\"9\")",
  "<lower> ::= (\"a\"|\"b\"|\"c\"|\"d\"|\"e\"|\"f\"|\"g\"|\"h\"|\"i\"|\"j\"|\"k\"|\"l\"|\"m\"|\"n\"|\"o\"|\"p\"|\"q\"|\"r\"|\"s\"|\"t\"|\"u\"|\"v\"|\"w\"|\"x\"|\"y\"|\"z\")",
  "<upper> ::= (\"A\"|\"B\"|\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"H\"|\"I\"|\"J\"|\"K\"|\"L\"|\"M\"|\"N\"|\"O\"|\"P\"|\"Q\"|\"R\"|\"S\"|\"T\"|\"U\"|\"V\"|\"W\"|\"X\"|\"Y\"|\"Z\")",
  "<comma> ::= \".\"",
  "<char> ::= (<number>|<lower>|<upper>|<comma>)",
  "<host> ::= <char> +",
  "<dir> ::= (<char>|\"/\") +",
  "<key> ::= (<lower>|<upper>)+",
  "<value> ::= (<lower>|<upper>|<number>)+",
  "<param> ::= <key> \"=\" <value>",
  "<params> ::= <param> (\"&\"<param>)*",
  "<scheme> ::= \"https\"|\"http\"",
  "<uri> ::= <scheme> \"://\" <host> [\"/\" <dir> [\"?\" <params>]]",
  "<root> ::= <uri>"
)
val inputs = List("https://github.com/user/project?abc=def&ghi=jkl")
val json_keys = List("scheme: scheme", "host: host", "key: key", "value: value")
Converter.jsonConvert(bnf_definition, inputs, json_keys).foreach {
  case (line, json) => {
    println(s"line number: ${line}")
    println(json)
  }
}
```
```
line number: 0
{"scheme":"https","host":"github.com","key":"abc,ghi","value":"def,jkl"}
```