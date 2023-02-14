# Notes
## General
- `n + k` patterns were banned in Haskell 2010 [1].
- `foldr` and `foldl` can be thought of non-recursively ($\oplus$ is an operator) [2]:

$$ foldr \: (\oplus) \:v \: [x_0, x_1, \dots, x_n] \:=\: x_0 \: \oplus \: (x_1 \: \oplus \: (\dots (x_n \: \oplus \: v) \dots )) $$

$$ foldl \: (\oplus) \:v \: [x_0, x_1, \dots, x_n] \:=\: (\dots (( v \: \oplus x_0) \: \oplus \: x_1) \dots) \: \oplus \: x_n $$

## References
[1] [https://stackoverflow.com/questions/20431106/parse-error-in-pattern-n-1](https://stackoverflow.com/questions/20431106/parse-error-in-pattern-n-1)<br/>
[2] [Hutton, G. (2007). Programming in Haskell. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511813672](https://doi.org/10.1017/CBO9780511813672)