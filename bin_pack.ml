
let reduce_left reducer ls =
    let first = List.hd ls in
    List.tl ls
    |> List.fold_left reducer first
;;

    
let min_by by =
    reduce_left (fun a b -> 
        if by a <= by b then a
        else b)
;;


let max_by by =
    reduce_left (fun a b -> 
        if by a >= by b then a
        else b)
;;


let rec except_one x = 
    function
    | [] -> []
    | (a::ls) when a = x -> ls
    | (a::ls) -> a::except_one x ls
;;


let sum_by by ls =
    List.map by ls 
    |> reduce_left (fun a b -> a + b) 
;;


type 'tag input_rectangle = 
    { w: int;
      h: int;
      tag: 'tag }
;;


type rectangle = 
    { x: int;
      y: int;
      w: int;
      h: int }
;;


type space = Space of rectangle;;


type 'tag result =
    { rects: (rectangle * 'tag) list;
      spaces: space list;
      width: int;
      height: int }
;;


let default_result =
    { rects = [];
      spaces = [];
      width = 0;
      height = 0 }
;;


let try_put_rect_into_space (rect: 'tag input_rectangle) (Space space) =
    let tag = rect.tag in
    
    let rect = 
        { x = space.x;
          y = space.y;
          w = rect.w;
          h = rect.h } in
          
    let space11 = 
        { space with
            x = space.x + rect.w;
            w = space.w - rect.w } in

    let space12 =
        { space with
            y = space.y + rect.h;
            h = space.h - rect.h;
            w = rect.w } in

    let space21 = 
        { space with
            y = space.y + rect.h;
            h = space.h - rect.h } in

    let space22 =
        { space with
            x = space.x + rect.w;
            w = space.w - rect.w;
            h = rect.h } in
 
    let space01, space02 =
        [ space11, space12; space21, space22 ]
        |> min_by (fun (x, _) -> abs (x.w - x.h)) in

    let valid =
        [ space01; space02 ]
        |> List.find_opt (fun x -> x.w < 0 || x.h < 0)
        |> function
            | None -> true
            | Some _ -> false in

    if not valid then None
    else
        let spaces = 
            [ space01; space02 ]
            |> List.filter (fun x -> x.w <> 0 && x.h <> 0)
            |> List.map (fun x -> Space x) in
            
        Some (rect, spaces, tag, Space space)
;;


let rec pack prev_result =
    function
    | [] -> prev_result
    | (current :: tail) -> 
        let put_results = 
            prev_result.spaces 
            |> List.filter_map (try_put_rect_into_space current) in
            
        match put_results with
        | [] ->
            let result =
                if current.w + prev_result.width > current.h + prev_result.height then
                    let new_width = max prev_result.width current.w in
                    { prev_result with 
                        width = new_width;
                        height = prev_result.height + current.h;
                        spaces = 
                            let space =
                                Space 
                                    { x = 0;
                                      y = prev_result.height;
                                      w = new_width;
                                      h = current.h } in

                            space :: prev_result.spaces }
                            
                else 
                    let new_height = max prev_result.height current.h in
                    { prev_result with 
                        width = prev_result.width + current.w;
                        height = new_height;
                        spaces =
                            let space =
                                Space 
                                    { x = prev_result.width;
                                      y = 0;
                                      w = current.w;
                                      h = new_height } in

                            space :: prev_result.spaces } in

            pack result (current :: tail)

        | put_results ->
            let rect, spaces, tag, org_space =
                put_results
                |> min_by (fun (_, spaces, _, _) ->
                    spaces |> sum_by (fun (Space a) -> a.w * a.h)) in
            
            let result =
                { prev_result with
                    rects = (rect, tag) :: prev_result.rects;
                    spaces = 
                        spaces @ (prev_result.spaces |> except_one org_space) } in
            
            pack result tail
;;


let bin_pack (input: 'a input_rectangle list) =
    let sort_by_w (x: 'a input_rectangle) (y: 'a input_rectangle) =
        y.w - x.w in
        
    let sort_by_h (x: 'a input_rectangle) (y: 'a input_rectangle) =
        y.h - x.h in
        
    let sort_by_area (x: 'a input_rectangle) (y: 'a input_rectangle) =
        y.h * y.w - x.h * x.w in
        
    let sort_by_long_edge (x: 'a input_rectangle) (y: 'a input_rectangle) =
        (max y.w y.h) - (max x.w x.h) in
        
    let sort_by_sum (x: 'a input_rectangle) (y: 'a input_rectangle) =
        (y.w + y.h) - (x.w + x.h) in
        
    [ input;
      List.fast_sort sort_by_w input;
      List.fast_sort sort_by_area input;
      List.fast_sort sort_by_h input;
      List.fast_sort sort_by_sum input;
      List.fast_sort sort_by_long_edge input ]
    |> List.map (pack default_result)
    |> max_by (fun r ->
        let all_area = r.width * r.height in
        let rect_area = r.rects |> sum_by (fun (x, _) -> x.w * x.h) in
        float rect_area /. float all_area)
;;

