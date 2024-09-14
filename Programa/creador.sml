use "auxiliares.sml";

(* Función para agregar un nuevo registro bancario *)
fun agregarRegistro archivo =
    let
        (* Solicitar datos al usuario *)
        val cuentaOrigen = 
            let
                val _ = TextIO.output (TextIO.stdOut, "Número de cuenta origen (4 digitos): ");
                val _ = TextIO.flushOut TextIO.stdOut
            in
                validarCuenta (pedirEntrada "")
            end

        (* Concatenar "ACC" a cuentaOrigen *)
        val cuentaOrigenConPrefijo = "ACC" ^ cuentaOrigen
        
        val fechaHora = 
            let
                val _ = TextIO.output (TextIO.stdOut, "Fecha y hora (YYYY-MM-DD HH:MM:SS): ");
                val _ = TextIO.flushOut TextIO.stdOut;
            in
                validarFechaHoraEntrada (pedirEntrada "")
            end
        
        val tipoTransaccion =
            let
                val _ = TextIO.output (TextIO.stdOut, "Tipo de transacción (1. Deposito, 2. Retiro, 3. Transferencia): ");
                val _ = TextIO.flushOut TextIO.stdOut;
            in
                validarTipoTransaccion (pedirEntrada "")
            end
        
        val monto =
        	let
		        val _ = TextIO.output (TextIO.stdOut, "Monto: ");
        		val _ = TextIO.flushOut TextIO.stdOut
        	in
        		validarMonto (pedirEntrada "")
			end

        (* CuentaDestino solo si el tipo de transacción es transferencia *)
        val cuentaDestinoConPrefijo = 
            if tipoTransaccion = "transferencia" then
                "ACC" ^ 
                let
                    val _ = TextIO.output (TextIO.stdOut, "Número de cuenta destino (4 digitos): ")
                    val _ = TextIO.flushOut TextIO.stdOut
                in
                    validarCuenta (pedirEntrada "")
                end
            else 
                ""


        (* Crear la línea para agregar al archivo solo si los datos ingresados son validos*)
        val linea = cuentaOrigenConPrefijo ^ "," ^ fechaHora ^ "," ^ tipoTransaccion ^ "," ^ monto ^ "," ^ cuentaDestinoConPrefijo

        (* Abrir el archivo y agregar la línea *)
        val _ = 
            let
                (* Abre el archivo en modo de escritura *)
                val out = TextIO.openAppend archivo
                val _ = TextIO.output (out, linea ^ "\n")
                val _ = TextIO.closeOut out
            in
                TextIO.output (TextIO.stdOut, "\nRegistro agregado exitosamente.\n")
            end
    in
        ()
    end;
    
(* Función para limpiar el índice (borrar todo el contenido del archivo) *)
fun limpiarIndice archivo =
    let
        (* Abre el archivo en modo de sobreescritura *)
        val out = TextIO.openOut archivo
        (* Cierra el archivo sin escribir nada (esto lo vacía) *)
        val _ = TextIO.closeOut out
    in
        TextIO.output (TextIO.stdOut, "\nÍndice limpiado exitosamente.\n")
    end

