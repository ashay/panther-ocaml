module Let_syntax = struct
  let return = Lwt.return

  let ( >>= ) = Lwt.Infix.( >>= )

  let ( >>| ) = Lwt.Infix.( >|= )

  module Let_syntax = struct
    let bind m ~f = Lwt.bind m f
  end
end
