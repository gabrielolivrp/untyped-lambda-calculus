# Untyped Lambda Calculus

### Repl

To open the repl, run the command:

```bash
$ stack run
```

### Example of using the repl

Creating a representation for the booleans values

```
Untyped> let true := λa. λb. a
Untyped> let false := λa. λb. b
```

Let's make a conditional structure: `if <expr> then <expr> else <expr>`

```
Untyped> let ifThenElse := λp. λa. λb. p a b
```

Executing the code `if true then zero else one`

```
Untyped> let zero := λs. λz. z
Untyped> let one := λs. λz. s z
Untyped> ifThenElse true zero one
```
