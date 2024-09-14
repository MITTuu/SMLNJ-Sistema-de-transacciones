use "auxiliares.sml";

(* Función para insertar un registro en la lista ordenada descendente por monto *)
fun insertarOrdenado ((cuenta_origen, fecha_hora, tipo_transaccion, monto: real, cuenta_destino), []) = 
    [(cuenta_origen, fecha_hora, tipo_transaccion, monto, cuenta_destino)]
  | insertarOrdenado ((cuenta_origen, fecha_hora, tipo_transaccion, monto: real, cuenta_destino), (x::xs)) =
    let
        val (_, _, _, monto_x: real, _) = x
    in
        if monto >= monto_x then
            (cuenta_origen, fecha_hora, tipo_transaccion, monto, cuenta_destino) :: x :: xs
        else
            x :: insertarOrdenado ((cuenta_origen, fecha_hora, tipo_transaccion, monto, cuenta_destino), xs)
    end

(* Función para ordenar los registros por monto en orden descendente *)
fun ordenarRegistrosPorMontoDescendente registros =
    foldl (fn (registro, acc) => insertarOrdenado (registro, acc)) [] registros

(* Función para mostrar los registros de forma tabular, la indentación está fallando *)
fun mostrarRegistrosTabular registros =
    let
        (* Formato de una línea para el encabezado *)
        val encabezado = "\nCuenta Origen\tFecha y Hora\t\t\tTransacción\t\tMonto\t\tCuenta Destino\n"

        (* Función para mostrar un registro individual *)
        fun mostrarRegistro (cuenta_origen, fecha_hora, tipo_transaccion, monto, cuenta_destino) =
            let
                (* Formato de una línea para los datos del registro *)
                val tipo_formateado = tipo_transaccion ^ (if tipo_transaccion = "retiro" then "\t\t\t" else "\t\t")
                val datos = cuenta_origen ^ "\t\t" ^
                            fecha_hora ^ "\t\t" ^
                            tipo_formateado ^
                            Real.toString monto ^ "\t\t" ^
                            cuenta_destino
            in
                print (datos ^ "\n" ^ "______________________________________________________________________________________________________\n")
            end
    in
        if List.null registros then ()
        else (
            print encabezado;
            List.app mostrarRegistro registros
        )
    end

(* Función principal que solicita los montos y filtra registros *)
fun filtrarRegistrosPorMonto registros =
    let
        (* Función interna para solicitar y validar los montos *)
        fun solicitarMontos () =
            let
                (* Solicitar y validar el monto de inicio *)
                val montoInicioStr = pedirEntrada "\nIngrese el monto de inicio: "
                val montoInicio = Real.fromString (validarMonto montoInicioStr)
                
                (* Solicitar y validar el monto final *)
                val montoFinalStr = pedirEntrada "\nIngrese el monto final: "
                val montoFinal = Real.fromString (validarMonto montoFinalStr)
            in
                (* Verificar que ambos montos son válidos y que el monto de inicio es menor al final *)
                case (montoInicio, montoFinal) of
                    (SOME inicio, SOME fin) =>
                        if inicio < fin then (inicio, fin)
                        else (
                            print "\nEl monto de inicio debe ser menor que el monto final. Por favor, inténtelo de nuevo.\n";
                            solicitarMontos ()  
                        )
                  | _ => solicitarMontos ()  
            end

        (* Obtener los del usuario *)
        val (inicio, fin) = solicitarMontos ()

        (* Filtrar los registros que están dentro del rango de montos *)
        val registrosFiltrados = 
            List.filter (fn (_, _, _, monto, _) => monto >= inicio andalso monto <= fin) registros
    in
        mostrarRegistrosTabular registrosFiltrados
    end

(* Función para separar la fecha de la hora en un registro *)
fun separarFechaHora fechaHora =
    let
        val partes = String.fields (fn c => c = #" ") fechaHora
    in
        case partes of
            [fecha, _] => fecha
          | _ => ""
    end

(* Función para buscar un valor en una lista de pares *)
fun buscarEnMapa clave mapa =
    case List.find (fn (k, _) => k = clave) mapa of
        SOME (_, valor) => valor
      | NONE => 0

(* Función para actualizar un valor en una lista de pares *)
fun actualizarMapa clave valor mapa =
    let
        (* Eliminar la entrada existente con la misma clave, si existe *)
        val mapaSinClave = List.filter (fn (k, _) => k <> clave) mapa
    in
        (clave, valor) :: mapaSinClave
    end

(* Función para contar transacciones por cuenta y día *)
fun contarTransaccionesPorCuenta registros =
    let
        (* Función auxiliar para agregar transacción a un mapa de cuenta-fecha *)
        fun agregarContador (cuenta, fecha, tipoTransaccion, mapa) =
            if tipoTransaccion = "retiro" orelse tipoTransaccion = "transferencia" then
                let
                    val clave = cuenta ^ "-" ^ fecha
                    val cantidad = buscarEnMapa clave mapa
                    val mapaActualizado = actualizarMapa clave (cantidad + 1) mapa
                in
                    mapaActualizado
                end
            else
                mapa

        (* Recorrer registros para contar transacciones *)
        fun recorrer registros mapa =
            case registros of
                [] => mapa
              | (cuentaOrigen, _, tipoTransaccion, _, fechaHora) :: tail =>
                    let
                        val fecha = separarFechaHora fechaHora
                        val mapaActualizado = agregarContador (cuentaOrigen, fecha, tipoTransaccion, mapa)
                    in
                        recorrer tail mapaActualizado
                    end
    in
        recorrer registros []
    end

(* Función para filtrar transacciones sospechosas *)
fun filtrarTransaccionesSospechosas registros =
    let
        (* Obtener el mapa con los contadores de transacciones por cuenta y fecha *)
        val contadorTransacciones = contarTransaccionesPorCuenta registros

        (* Filtrar registros que tienen 5 o más transacciones por cuenta en un día *)
        val registrosSospechosos =
            List.filter (fn (cuentaOrigen, _, tipoTransaccion, _, fechaHora) =>
                if tipoTransaccion = "retiro" orelse tipoTransaccion = "transferencia" then
                    let
                        val fecha = separarFechaHora fechaHora
                        val clave = cuentaOrigen ^ "-" ^ fecha
                        val cantidad = buscarEnMapa clave contadorTransacciones
                    in
                        cantidad >= 5
                    end
                else
                    false
            ) registros
    in
        mostrarRegistrosTabular registrosSospechosos
    end

(* Función para filtrar transacciones por cuenta *)
fun filtrarTransaccionesPorCuenta registros cuentaIngresada =
    let
        (* Filtrar los registros que corresponden al número de cuenta ingresado *)
        val registrosPorCuenta = 
            List.filter (fn (cuentaOrigen, _, _, _, cuentaDestino) =>
                cuentaOrigen = cuentaIngresada orelse cuentaDestino = cuentaIngresada) registros
    in
        mostrarRegistrosTabular registrosPorCuenta
    end

(* Función para contar transacciones por tipo *)
fun contarTransaccionesPorTipo registros tipoIngresado =
    let
        (* Filtrar los registros que corresponden al tipo de transacción ingresado *)
        val transaccionesPorTipo = 
            List.filter (fn (_, _, tipo, _, _) =>
                tipo = tipoIngresado) registros
        
        (* Contar el número de transacciones del tipo especificado *)
        val cantidadTransacciones = List.length transaccionesPorTipo
    in
        cantidadTransacciones
    end

(* Función para mostrar el resumen de transacciones *) 
fun resumen registros =
    let
        (* Función interna para mostrar el resumen de transacciones *)
        fun mostrarResumenTransacciones () =
            let
                (* Contar las transacciones por cada tipo *)
                val cantidadDepositos = contarTransaccionesPorTipo registros "deposito"
                val cantidadRetiros = contarTransaccionesPorTipo registros "retiro"
                val cantidadTransferencias = contarTransaccionesPorTipo registros "transferencia"

                (* Mostrar el resumen *)
                val _ = TextIO.output (TextIO.stdOut, "\nResumen de transacciones\n\n")
                val _ = TextIO.output (TextIO.stdOut, "Tipo de transacción  |  Cantidad\n")
                val _ = TextIO.output (TextIO.stdOut, "---------------------|-----------------\n")
                val _ = TextIO.output (TextIO.stdOut, "Deposito             |  " ^ Int.toString cantidadDepositos ^ "\n")
                val _ = TextIO.output (TextIO.stdOut, "Retiro               |  " ^ Int.toString cantidadRetiros ^ "\n")
                val _ = TextIO.output (TextIO.stdOut, "Transferencia        |  " ^ Int.toString cantidadTransferencias ^ "\n")
            in
                ()
            end

        (* Función para obtener las transacciones con el monto mayor y menor *)
        fun transaccionesExtremas () =
            case registros of
                [] => (NONE, NONE)  (* Si la lista está vacía *)
              | registrosOrdenados =>
                    let
                        val transaccionMaxima = List.hd registrosOrdenados  (* Primer registro *)
                        val transaccionMinima = List.last registrosOrdenados  (* Último registro *)
                    in
                        (SOME transaccionMaxima, SOME transaccionMinima)
                    end

        (* Obtener el registro con el monto mayor y menor *)
        val (transaccionMayor, transaccionMenor) = transaccionesExtremas ()

        (* Función para mostrar las transacciones extremas *)
        fun mostrarTransaccionesExtremas () =
            let
                val _ = case transaccionMayor of
                            NONE => TextIO.output (TextIO.stdOut, "\nNo hay transacciones.\n")
                          | SOME (cuentaOrigen, fecha, tipo, monto, cuentaDestino) => 
                                TextIO.output (TextIO.stdOut, 
                                    "\nTransacción con el monto mayor:\n" ^ 
                                    "Cuenta Origen: " ^ cuentaOrigen ^ "\n" ^ 
                                    "Fecha: " ^ fecha ^ "\n" ^ 
                                    "Tipo: " ^ tipo ^ "\n" ^ 
                                    "Monto: " ^ Real.toString monto ^ "\n" ^ 
                                    (if tipo = "transferencia" then "Cuenta Destino: " ^ cuentaDestino ^ "\n" else ""))

                val _ = case transaccionMenor of
                            NONE => TextIO.output (TextIO.stdOut, "")
                          | SOME (cuentaOrigen, fecha, tipo, monto, cuentaDestino) => 
                                TextIO.output (TextIO.stdOut, 
                                    "\nTransacción con el monto menor:\n" ^ 
                                    "Cuenta Origen: " ^ cuentaOrigen ^ "\n" ^ 
                                    "Fecha: " ^ fecha ^ "\n" ^ 
                                    "Tipo: " ^ tipo ^ "\n" ^ 
                                    "Monto: " ^ Real.toString monto ^ "\n" ^ 
                                    (if tipo = "transferencia" then "Cuenta Destino: " ^ cuentaDestino ^ "\n" else ""))
            in
                ()
            end

        (* Función para encontrar la cuenta con el mayor monto recibido en transferencias *)
        fun cuentaConMayorMontoRecibido () =
            let
                (* Filtrar las transacciones de tipo transferencia *)
                fun filtrarTransferencias [] = []
                  | filtrarTransferencias ((_, _, "transferencia", monto, cuentaDestino) :: tail) =
                        (cuentaDestino, monto) :: filtrarTransferencias tail
                  | filtrarTransferencias (_ :: tail) = filtrarTransferencias tail

                (* Obtener todas las transferencias *)
                val transferencias = filtrarTransferencias registros

                (* Función para sumar los montos recibidos por cuenta *)
                fun sumarMontosPorCuenta [] mapa = mapa
                  | sumarMontosPorCuenta ((cuenta, monto) :: tail) mapa =
                        let
                            val montoActual = case List.find (fn (c, _) => c = cuenta) mapa of
                                                 NONE => 0.0
                                               | SOME (_, montoExistente) => montoExistente
                            val mapaActualizado = (cuenta, montoActual + monto) :: List.filter (fn (c, _) => c <> cuenta) mapa
                        in
                            sumarMontosPorCuenta tail mapaActualizado
                        end

                (* Obtener el mapa con la suma de montos por cuenta *)
                val mapaMontos = sumarMontosPorCuenta transferencias []

                (* Función para encontrar la cuenta con el mayor monto recibido *)
                fun encontrarMaximoMonto ([], cuentaMax, maxMonto) = (cuentaMax, maxMonto)
                  | encontrarMaximoMonto ((cuenta, monto) :: tail, cuentaMax, maxMonto) =
                        if monto > maxMonto then
                            encontrarMaximoMonto (tail, cuenta, monto)
                        else
                            encontrarMaximoMonto (tail, cuentaMax, maxMonto)

                (* Inicializar la búsqueda con un valor por defecto *)
                val (cuentaMax, maxMonto) = encontrarMaximoMonto (mapaMontos, "", 0.0)
            in
                (cuentaMax, maxMonto)
            end

        (* Función para mostrar la cuenta con el mayor monto recibido *)
        fun mostrarCuentaConMayorMontoRecibido () =
            let
                val (cuentaMax, maxMonto) = cuentaConMayorMontoRecibido ()
            in
                if maxMonto > 0.0 then
                    TextIO.output (TextIO.stdOut, "\nLa cuenta con el mayor monto recibido es: " ^ cuentaMax ^ " con " ^ Real.toString maxMonto ^ " recibidos.\n")
                else
                    TextIO.output (TextIO.stdOut, "\nNo se encontraron transferencias.\n")
            end

        (* Función para encontrar la cuenta con más transacciones *)
        fun cuentaConMasTransacciones registros =
            let
                (* Obtener el mapa con la cuenta y el número de transacciones *)
                val mapaCuentas = contarTransaccionesPorCuenta registros

                (* Función para comparar cuentas y encontrar la de mayor cantidad de transacciones *)
                fun encontrarMaximaCuenta ([], cuentaMax, maxTransacciones) = (cuentaMax, maxTransacciones)
                  | encontrarMaximaCuenta ((cuenta, transacciones) :: tail, cuentaMax, maxTransacciones) =
                        if transacciones > maxTransacciones then
                            encontrarMaximaCuenta (tail, cuenta, transacciones)
                        else
                            encontrarMaximaCuenta (tail, cuentaMax, maxTransacciones)

                (* Inicializar la búsqueda con un valor por defecto *)
                val (cuentaMax, maxTransacciones) = encontrarMaximaCuenta (mapaCuentas, "", 0)
            in
                (cuentaMax, maxTransacciones)
            end

        (* Función para mostrar la cuenta con más transacciones *)
        fun mostrarCuentaConMasTransacciones registros =
            let
                val (cuentaMax, maxTransacciones) = cuentaConMasTransacciones registros
            in
                if maxTransacciones > 0 then
                    TextIO.output (TextIO.stdOut, "\nLa cuenta con más transacciones es: " ^ cuentaMax ^ " con " ^ Int.toString maxTransacciones ^ " transacciones.\n")
                else
                    TextIO.output (TextIO.stdOut, "\nNo se encontraron transacciones.\n")
            end

    in
        mostrarResumenTransacciones ();
        mostrarTransaccionesExtremas ();
        mostrarCuentaConMasTransacciones registros;
        mostrarCuentaConMayorMontoRecibido ()
    end








