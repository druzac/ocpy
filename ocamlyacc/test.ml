type day = Mon | Tues
type month = Jan | Feb
type date = Day of day | Month of month

let mon = Mon;;

assert (mon = Tues)
