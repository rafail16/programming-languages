open Array2

type cell = {r:int,c:int,pr:int,pc:int,dist:int};
type zeugos = {r:int,c:int,move:char};

val dest_r = ref 0;
val dest_c = ref 0;

val res:int ref = ref 0;

fun solve(input) =
    let
        val (rows,cols_tmp):int*int = Array2.dimensions(input);
        val cols:int = cols_tmp-1;
        val rows_garbage:int =rows-1 ;
        val catQ = Queue.mkQueue():( int*int*int*int*int*char ) Queue.queue;
        val wQ = Queue.mkQueue():( int*int*int*int*int*char ) Queue.queue;
        val arrival_time = Array2.array(rows,cols+1,~1);

        val airports = Queue.mkQueue() :(int*int) Queue.queue;
    val sotarr = Array2.array(rows,cols+1,~1)
    val fAir_x = ref ~1;
        val fAir_y = ref ~1;
        val visited = Array2.array(rows,cols+1,~1);
        val previous = Array2.array(rows,cols+1,(~1,~1,#"."));
        val res = ref 0;
        val res_r = ref 0;
        val res_c = ref 0;
        val fin_r = ref 0;
        val fin_c = ref 0;
        val fAir_dist = ref 0;
        fun zero (x:int):int = 0;

        fun p(r,c) =
            if r = !dest_r andalso c = !dest_c then ()
            else
            let
                val z = Array2.sub(previous,r,c);
            in
                p(#1 z, #2 z);
                print(Char.toString (#3 z))
            end;

            fun printme (r,c,counter)  =
                if r = !dest_r andalso c = !dest_c then print(Int.toString counter ^ "\n")
                else
                let
                    val z = Array2.sub(previous,r,c);
                    val q = counter +1;
                in
                    printme (#1 z, #2 z,q)
                end;

        fun init(~1,_) = ()
        | init(r:int,c:int) =
        let
            val ch = Array2.sub(input,r,c);
            val next_r:int = if c = 0 then r-1 else r;
            val next_c:int = if c = 0 then cols else c-1;
            val newr=rows-1-r
        in
            res_r := rows-1;
            if ch = #"S" then(
                Queue.enqueue(catQ,(r,c,~1,~1,0,#"n"));
                dest_r := r;
                dest_c := c;
                Array2.update(input,r,c,#".");

                init(next_r,next_c)
            )else if ch = #"T" then(
                fin_r := r ;
                fin_c := c;
                Array2.update(input,r,c,#".");
                init(next_r,next_c)
            )
            else if ch = #"W" then(
                Queue.enqueue(wQ,(r,c,~1,~1,0,#"n"));
                Array2.update(input,r,c,#".");
                init(next_r,next_c)
            )else if ch = #"A" then (
                Queue.enqueue(airports,(r,c));

                init(next_r,next_c)
            )
            else
                init(next_r,next_c)
        end;

        fun up_pos(Q,r:int,c:int,V:int,H:int,dist:int,M) =
            if r+V >= 0 andalso c+H >= 0 andalso r+V < rows andalso c+H < cols then
                if	(Array2.sub(input,r+V,c+H) = #"." orelse Array2.sub(input,r+V,c+H) = #"A" )andalso Array2.sub(visited,r+V,c+H) = 0  andalso (Array2.sub(arrival_time,r+V,c+H) > dist+1 orelse Array2.sub(arrival_time,r+V,c+H) = ~1 ) then
                    Queue.enqueue(Q,(r+V,c+H,r,c,dist+1,M))
                else
                    ()
            else
                ()


        fun pushairports()  =
                if Queue.isEmpty(airports) then ()
                else
                    let
                        val front = Queue.dequeue(airports);
                        val r = #1 front;
                        val c = #2 front;
                        val time = !fAir_dist +5;
                        val rx= !fAir_x;
                        val ry= !fAir_y;

                    in
                      if (Array2.sub(arrival_time,r,c) > time+5 orelse Array2.sub(arrival_time,r,c) = ~1) andalso r <> rx andalso c <> ry then (
                            Queue.enqueue(wQ,(r,c,~1,~1,time,#"n"));
                            pushairports()
                        )
                        else pushairports()
                    end;


         fun up_pos1(Q,r:int,c:int,V:int,H:int,dist:int,M) =
             if r+V >= 0 andalso c+H >= 0 andalso r+V < rows andalso c+H < cols then
                    if	Array2.sub(input,r+V,c+H) = #"."  andalso Array2.sub(visited,r+V,c+H) = ~1 then (
                            Queue.enqueue(Q,(r+V,c+H,r,c,dist+2,M))
                    )
                    else if Array2.sub(input,r+V,c+H) = #"A" andalso !fAir_x = ~1 andalso !fAir_y = ~1 then (
                           fAir_x := r+V;
                             fAir_y := c+H;
                             fAir_dist := dist+2;
                             pushairports();
                             Queue.enqueue(Q,(r+V,c+H,r,c,dist+2,M))
                        )
                    (*						    Queue.enqueue(Q,(r+V,c+H,r,c,dist+2,M))
                        *)
                    else if Array2.sub(input,r+V,c+H) = #"A" andalso !fAir_x <> ~1 andalso !fAir_y <> ~1  andalso Array2.sub(visited,r+V,c+H) = ~1 then (
                            if !fAir_dist+5 < dist+2 then Queue.enqueue(Q,(r+V,c+H,r,c,!fAir_dist+5,M))
                                else Queue.enqueue(Q,(r+V,c+H,r,c,dist+2,M))
                    )
                    else if Array2.sub(input,r+V,c+H) = #"A" andalso !fAir_x <> ~1 andalso !fAir_y <> ~1  andalso Array2.sub(visited,r+V,c+H) <> ~1 then(
                             if !fAir_dist+5 < Array2.sub(visited,r+V,c+H) then Queue.enqueue(Q,(r+V,c+H,r,c,!fAir_dist+5,M))
                             else ()
                    )
                 else
                            ()
             else
                        ()





        fun bfs_w() =
        if Queue.isEmpty(wQ) then ()
        else
            let
                val front = Queue.dequeue(wQ);
                val r = #1 front;
                val c = #2 front;
                val dist = #5 front;
            in
                if Array2.sub(visited,r,c) <> ~1 then
                    bfs_w()
                else(
                    Array2.update(visited,r, c,dist);
                    up_pos1(wQ,r,c,~1,0,dist,#"D");
                    up_pos1(wQ,r,c,0,~1,dist,#"L");
                    up_pos1(wQ,r,c,0,1,dist,#"R");
                    up_pos1(wQ,r,c,1,0,dist,#"U");
                    (*print(Int.toString Array2.sub(input,r+V,c+H)) *)
                    if dist < Array2.sub(arrival_time,r,c) orelse Array2.sub(arrival_time,r,c) = ~1  then (Array2.update(arrival_time,r,c,dist);
                     bfs_w()
                    )
                    else
                     bfs_w()

                )
            end;
            fun bfs_sot() =
            if Queue.isEmpty(catQ) then ()
            else
                let
                    val front = Queue.dequeue(catQ);
                    val r = #1 front;
                    val c = #2 front;
                    val pr = #3 front;
                    val pc = #4 front;
                    val dist = #5 front;
                    val M = #6 front;
                    val ar_time:int = Array2.sub(arrival_time,r,c);
                    val at:int = ar_time-1;
                    val test=Array2.sub(arrival_time,r,c);
                in
                    if Array2.sub(visited,r,c) = 1 then
                        bfs_sot()
                    else(
                        Array2.update(visited,r, c,1);

                        up_pos(catQ,r,c,~1,0,dist,#"D");
                        up_pos(catQ,r,c,0,~1,dist,#"L");
                        up_pos(catQ,r,c,0,1,dist,#"R");
                        up_pos(catQ,r,c,1,0,dist,#"U");
                        (*print(Int.toString Array2.sub(input,r+V,c+H))
                        print(Int.toString r ^ Int.toString c ^ " " ^Int.toString dist ^ "\n"); *)
                        Array2.update(previous,r,c,(pr,pc,M));
                        Array2.update(sotarr,r,c,dist);
                        if r = !fin_r andalso c= !fin_c then (
                            res := 1;
                            bfs_sot()
                        )
                        else bfs_sot()
                    )
                end;
   val finalr = !fin_r;
     val finalc = !fin_c;
   val count=0;
    in
        init(rows-1,cols);
        bfs_w();
        Array2.modify RowMajor zero visited ;
        bfs_sot();
        if !res = 1 then (
            printme(!fin_r,!fin_c,count);
            p(!fin_r,!fin_c);
            print("\n")
        )
        else print("IMPOSSIBLE\n")

    end;

fun parse file =
    let
        val stream = TextIO.openIn file;
        fun to_char s = String.explode(Option.valOf s);
        fun read_input (stream,l) =
            let
                val line = TextIO.inputLine stream
            in
                if line = NONE then
                    l
                else
                    read_input(stream, to_char(line)::l)
            end;
    in
        Array2.fromList (read_input(stream,[]))
    end;

fun stayhome f = solve(parse(f));