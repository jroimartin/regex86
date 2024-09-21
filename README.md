# regexp

**DISCLAIMER:** This is a toy implementation created for learning
purposes.

Toy regexp engine.

The only supported metacharacters are `* + ? | ( )`.  Their
precedence, from weakest to strongest binding, is first alternation
(`|`), then concatenation, and finally the repetition operators (`*`,
`+` and `?`).
