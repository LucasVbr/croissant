class evalEnvironment =
  object
    val mutable env : (string, Literals.literal_value) Hashtbl.t =
      Hashtbl.create 10

    method add (name : string) (lit : Literals.literal_value) =
      Hashtbl.add env name lit

    method find (name : string) = Hashtbl.find env name

    method clone : evalEnvironment =
      let new_env = new evalEnvironment in
      Hashtbl.iter (fun k v -> new_env#add k v) env;
      new_env
  end

class typeEnvironments =
  object
    val mutable env : (string, Types.t) Hashtbl.t = Hashtbl.create 10
    method add (name : string) (tp : Types.t) = Hashtbl.add env name tp
    method find (name : string) = Hashtbl.find env name

    method clone : typeEnvironments =
      let new_env = new typeEnvironments in
      Hashtbl.iter (fun k v -> new_env#add k v) env;
      new_env
  end