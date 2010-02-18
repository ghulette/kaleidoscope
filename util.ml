type 'a either = 
  | Ok of 'a 
  | Error of string
  
let banner msg =
  let fill_char = '-' in
  let banner_width = 80 in
  let msg_width = (String.length msg) + 2 in
  let ban = if banner_width <= msg_width then msg else
    let total_fill_w = banner_width - msg_width in
    let left_fill_w = total_fill_w / 2 in
    let right_fill_w = total_fill_w - left_fill_w in
    let left = String.make left_fill_w fill_char in
    let right = String.make right_fill_w fill_char in
    left ^ " " ^ msg ^ " " ^ right
  in 
    print_string ban;
    flush stdout
