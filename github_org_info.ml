open Core.Std

let print_org file () =
    let url = sprintf "https://api.github.com/org/%s" file in
    Core_extended.Shell.run_full "curl" [url]
    |> Github_org_j.org_of_string
    |> fun org ->
            let open Github_org_t in
            let name = Option.value ~default:"???" org.name in
            printf "%s (%d) with %d public repos\n"
                name org.id org.public_repos

let () =
    Command.basic ~summary:"GitHub organization summary"
        Command.Spec.(empty +> anon ("org" %: string))
        print_org
    |> Command.run
