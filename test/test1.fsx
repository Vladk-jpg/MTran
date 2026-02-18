let a, ua = 55y, 55uy
let b, ub = 50s, 50us
let mutable c, uc = 50, 50u
let d, ud = 50L, 50uL
let bigInt = 9999999999999I
let e = 50.0f
let mutable f = 50.0
let decimal = 50.0m
let str = "text"
let ch = 'a'
let boolVal = true
c <- 67
let n = 4 * c
let newStr = str + " value"

let mutable x = 1
while x < 5 do
  x <- x + 1

let add (a: int) (b: int): int = a + b
let mutable res = 0
for i = 0 to 10 do
  res <- add res i

let getAnswer () = 42
for j in 11 .. 14 do
  res <- add res (getAnswer())

let list1 = [1; 2; 3; 4; 5; 6]
let arr = [|1; 2; 3|]

let processNumbers =
  List.filter (fun x -> x % 2 = 0)
  >> List.map (fun x -> x * x)
  >> List.sum

let result = processNumbers list1;

type Person = { Name: string; Age: int }
let person = { Name = "Vlad"; Age = 20 }

let checkAge =
  if person.Age > 18 then
    "You are adult"
  else
    "You are kid"

let nameCheck =
  match person.Name with
  | "Vlad" -> true
  | "Vladimir" -> true
  | _ -> false