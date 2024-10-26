type literal_value =
  [ `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Char of char
  | `Null
  | `Void ]

let string_of_literal_value = function
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ s ^ "\""
  | `Bool b -> string_of_bool b
  | `Char c -> "'" ^ String.make 1 c ^ "'"
  | `Null -> "null"
  | `Void -> "void"

class virtual literal =
  object
    inherit Node.node
    method virtual value : literal_value
    method virtual check_type : Types.t
  end

class eInteger (value : int) =
  object
    inherit literal
    method value = `Int value
    method to_string = "Integer(" ^ string_of_int value ^ ")"
    method check_type = Types.IntegerType
  end

class eFloat (value : float) =
  object
    inherit literal
    method value = `Float value
    method to_string : string = "Float(" ^ string_of_float value ^ ")"
    method check_type = Types.FloatType
  end

class eString (value : string) =
  object
    inherit literal
    method value = `String value
    method to_string = "String(\"" ^ value ^ "\")"
    method check_type = Types.StringType
  end

class eBoolean (value : bool) =
  object
    inherit literal
    method value = `Bool value
    method to_string = "Boolean(" ^ string_of_bool value ^ ")"
    method check_type = Types.BooleanType
  end

class eCharacter (value : char) =
  object
    inherit literal
    method value = `Char value
    method to_string = "Character(" ^ String.make 1 value ^ ")"
    method check_type = Types.CharacterType
  end

class eNull =
  object
    inherit literal
    method value = `Null
    method to_string = "Null"
    method check_type = Types.NullType
  end

class eVoid =
  object
    inherit literal
    method value = `Void
    method to_string = "Void"
    method check_type = Types.VoidType
  end