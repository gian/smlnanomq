signature PRIM_NMQ =
sig
    type context
    type node
    type uint = Word32.word

    val nmq_context : string -> context
    val nmq_create : context * uint * uint * uint -> context
    val nmq_open : context * uint * uint * uint -> context
    val nmq_print : context -> unit
    val nmq_close : context -> unit
    val nmq_node : context * uint -> node
    val nmq_send : node * uint * char array * uint -> bool
    val nmq_sendnb : node * uint * char array * uint -> bool
    val nmq_recvfrom : node * uint * char array * uint ref -> bool
    val nmq_recvfromnb : node * uint * char array * uint ref -> bool
    val nmq_recv : node * char array * uint ref -> bool
    val nmq_recvnb : node * char array * uint ref -> bool

    val validContext : context -> bool
    val validNode : node -> bool
end

structure PrimNMQ :> PRIM_NMQ =
struct
    type context = MLton.Pointer.t
    type node = MLton.Pointer.t
    type uint = Word32.word

    val nmq_context = _import "sml_nmq_context" public: string -> context;
    val nmq_create = _import "sml_nmq_create" public: context * uint * uint * uint -> context;
    val nmq_open = _import "sml_nmq_open" public: context * uint * uint * uint -> context;
    val nmq_print = _import "sml_nmq_print" public: context -> unit;
    val nmq_close = _import "sml_nmq_close" public: context -> unit;
    val nmq_node = _import "sml_nmq_node" public: context * uint -> node;
    val nmq_send = _import "sml_nmq_send" public: node * uint * char array * uint -> bool;
    val nmq_sendnb = _import "sml_nmq_sendnb" public: node * uint * char array * uint -> bool;
    val nmq_recvfrom = _import "sml_nmq_recvfrom" public: node * uint * char array * uint ref -> bool;
    val nmq_recvfromnb = _import "sml_nmq_recvfromnb" public: node * uint * char array * uint ref -> bool;
    val nmq_recv = _import "sml_nmq_recv" public: node * char array * uint ref -> bool;
    val nmq_recvnb = _import "sml_nmq_recvnb" public: node * char array * uint ref -> bool;

    (* Should check for the underlying pointer being null. *)
    fun validContext c = c <> MLton.Pointer.null
    fun validNode n = n <> MLton.Pointer.null
end

signature NANOMQ =
sig
    type context
    type node
    type node_id = int

    exception NanoMQError of string

    (** Create a context.
        
        Parameters:
         Path to the context file.
         Nodes
         Size
         messageSize **)
    val createContext : string * int * int * int -> context

    (** Open an existing context.
        
        Parameters:
         Path to the context file.
         Nodes
         Size
         Message Size **)
    val openContext : string * int * int * int -> context

    (** Print information about a context **)
    val print : context -> unit

    (** Close a context, freeing the underlying resources. **)
    val close : context -> unit

    (** Create a node reference.

        Parameters:
         Pre-opened/created context
         Node ID **)
    val node : context * node_id -> node

    (** Send a message.

        Parameters:
         Our local node.
         remote node id
         An array of size < message_size.
    **)
    val send : node * node_id * char array -> unit
    val sendNB : node * node_id * char array -> unit

    (** Receive a message from a particular peer. 
        
        Parameters:
         Local node.
         Remote node ID
         
        Returns:
         A vector of size at most messageSize **)
    val recvFrom : node * node_id -> char vector
    val recvFromNB : node * node_id -> char vector option

    (** Receive a message from any peer. 
        Parameters:
         Local node.
         
        Returns:
         A vector of size at most messageSize **)

    val recv : node -> char vector
    val recvNB : node -> char vector option 
end

structure NanoMQ :> NANOMQ =
struct
    type context = string * Word32.word * PrimNMQ.context
    type node = context * PrimNMQ.node
    type node_id = int

    exception NanoMQError of string

    val w32 = Word32.fromInt
    val i32 = Word32.toInt

    fun check s false = raise NanoMQError s
      | check s true = ()

    fun createContext (file,nodes,size,messageSize) = 
        let
            val ctx = PrimNMQ.nmq_context file

            val _ = if not (PrimNMQ.validContext ctx)
                        then raise NanoMQError "Could not initalise context" else ()

            val ctx' = PrimNMQ.nmq_create (ctx, w32 nodes, w32 size, w32 messageSize)

            val _ = if not (PrimNMQ.validContext ctx')
                        then raise NanoMQError "Could not create context" else ()
        in
            (file, Word32.fromInt messageSize, ctx') 
        end

    fun openContext (file,nodes,size,messageSize) = 
        let
            val ctx = PrimNMQ.nmq_context file

            val _ = if not (PrimNMQ.validContext ctx)
                        then raise NanoMQError "Could not initalise context" else ()

            val ctx' = PrimNMQ.nmq_open (ctx, w32 nodes, w32 size, w32 messageSize)

            val _ = if not (PrimNMQ.validContext ctx')
                        then raise NanoMQError "Could not open context" else ()
        in
            (file, w32 messageSize, ctx') 
        end

    fun print (_,_,ctx) = PrimNMQ.nmq_print ctx

    fun close (_,_,ctx) = PrimNMQ.nmq_close ctx (* TODO: Consider deleting the file *) 

    fun node (c as (_,_,ctx),id) = (c,PrimNMQ.nmq_node (ctx, w32 id))

    (** Send a message.

        Parameters:
         Our local node.
         remote node id
         An array of size < message_size.
    **)
    fun send ((_,node),id,arr) = check "send" (PrimNMQ.nmq_send (node, w32 id, arr, w32 (Array.length arr)))
    fun sendNB ((_,node),id,arr) = check "sendNB" (PrimNMQ.nmq_sendnb (node, w32 id, arr, w32 (Array.length arr)))

    (** Receive a message from a particular peer. 
        
        Parameters:
         Local node.
         Remote node ID
         
        Returns:
         A vector of size at most messageSize **)
    fun recvFrom ((ctx,node),id) =
        let
            val (_,mSz,ctx') = ctx
            val buf = Array.array(i32 mSz, Char.chr 0)
            val recvSz = ref mSz
            val _ = check "recvFrom" (PrimNMQ.nmq_recvfrom (node, w32 id, buf, recvSz))
        in
            VectorSlice.vector (VectorSlice.slice (Array.vector buf, 0, SOME (i32 (!recvSz))))
        end

    fun recvFromNB ((ctx,node),id) =
        let
            val (_,mSz,ctx') = ctx
            val buf = Array.array(i32 mSz, Char.chr 0)
            val recvSz = ref mSz
            val _ = check "recvFromNB" (PrimNMQ.nmq_recvfromnb (node, w32 id, buf, recvSz))
        in
            if i32 (!recvSz) > 0 then
                SOME (VectorSlice.vector (VectorSlice.slice (Array.vector buf, 0, SOME (i32 (!recvSz)))))
            else NONE
        end


    (** Receive a message from any peer. 
        Parameters:
         Local node.
         
        Returns:
         A vector of size at most messageSize **)

    fun recv (ctx,node) =
        let
            val (_,mSz,ctx') = ctx
            val buf = Array.array(i32 mSz, Char.chr 0)
            val recvSz = ref mSz
            val _ = check "recv" (PrimNMQ.nmq_recv (node, buf, recvSz))
        in
            VectorSlice.vector (VectorSlice.slice (Array.vector buf, 0, SOME (i32 (!recvSz))))
        end

    fun recvNB  (ctx,node) =
        let
            val (_,mSz,ctx') = ctx
            val buf = Array.array(i32 mSz, Char.chr 0)
            val recvSz = ref mSz
            val _ = check "recvNB" (PrimNMQ.nmq_recvnb (node, buf, recvSz))
        in
            if i32 (!recvSz) > 0 then
                SOME (VectorSlice.vector (VectorSlice.slice (Array.vector buf, 0, SOME (i32 (!recvSz)))))
            else NONE
        end
end

fun testSend () =
    let
        val ctx = NanoMQ.openContext ("/tmp/nmq3", 2, 15, 100)
        val node = NanoMQ.node (ctx,1)
        val a = Array.array (100, #".")
        val _ = NanoMQ.send (node, 0, a)
    in
        ()
    end

fun testRecv () =
    let
        val ctx = NanoMQ.createContext ("/tmp/nmq3", 2, 15, 100)
        val node = NanoMQ.node (ctx,0)
        val v = NanoMQ.recv node
        val _ = print ("Length: " ^ Int.toString (Vector.length v) ^ "\n")
        val _ = Vector.appi (fn (i,v) => print (Int.toString i ^ ": " ^ str v ^ "\n")) v
    in
        ()
    end




(*
fun testSend () =
    let
        val ctx = PrimNmq.nmq_context "/tmp/nmq3"
        val ctx' = PrimNmq.nmq_open (ctx, 0w2, 0w15, 0w100)
        val node = PrimNmq.nmq_node (ctx',0w1)
        val a = Array.array (100, #".")
        val _ = PrimNmq.nmq_send (node, 0w0, a, 0w100)
    in
        ()
    end

fun testRecv () =
    let
        val ctx = PrimNmq.nmq_context "/tmp/nmq3"
        val ctx' = PrimNmq.nmq_create (ctx, 0w2, 0w15, 0w100)
        val node = PrimNmq.nmq_node (ctx',0w0)
        val a = Array.array (101, #"*")
        val sz = ref 0w101
        val _ = PrimNmq.nmq_recvfrom (node, 0w1, a, sz)
        val _ = print ("Received: " ^ LargeInt.toString (Word32.toLargeInt (!sz)) ^ " bytes\n")
        val _ = print ("Length: " ^ Int.toString (Array.length a) ^ "\n")
        val _ = print ("Array contents:\n")
        val _ = Array.appi (fn (i,v) => print (Int.toString i ^ ": " ^ str v ^ "\n")) a
    in
        ()
    end

*)

