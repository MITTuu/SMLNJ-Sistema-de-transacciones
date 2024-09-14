(* Función para eliminar espacios en blanco al final *)
fun leerEntrada (str: string) =
    let
        val len = String.size str
    in
        if len > 0 andalso String.sub (str, len - 1) = #"\n"
        then String.substring (str, 0, len - 1)
        else str
    end					 							 
    
(* Función para leer la entrada del usuario *)
fun pedirEntrada prompt =
    let
        val _ = TextIO.output (TextIO.stdOut, prompt);
        val _ = TextIO.flushOut TextIO.stdOut;
        val input = 
            case TextIO.inputLine TextIO.stdIn of
                SOME line => leerEntrada line
              | NONE     => ""
    in
        input
    end

(* Función para leer el archivo CSV y devolver una lista de registros *)
fun leerArchivoCSV ruta =
    let
        (* Función auxiliar para dividir una línea en campos *)
        fun dividirCampos linea =
            String.tokens (fn c => c = #",") linea

        (* Función auxiliar para procesar cada línea del archivo *)
        fun procesarLinea linea =
            let
                val campos = dividirCampos linea
            in
                (* Verifica que la línea tenga el número correcto de campos *)
                if length campos >= 4 andalso length campos <= 5 then
                    (* Convierte el monto a un número real *)
                    let
                        val cuenta_origen = hd campos
                        val fecha_hora = hd (tl campos)
                        val tipo_transaccion = hd (tl (tl campos))
                        val montoOpt = Real.fromString (hd (tl (tl (tl campos))))
                        val cuenta_destino = if length campos = 5 then SOME (hd (tl (tl (tl (tl campos))))) else NONE
                    in
                        case montoOpt of
                            SOME monto => SOME (cuenta_origen, fecha_hora, tipo_transaccion, monto, 
                                                 case cuenta_destino of
                                                     SOME cd => cd
                                                   | NONE => "")
                          | NONE => NONE
                    end
                else
                    NONE
            end

        (* Lee el archivo línea por línea y lo procesa *)
        fun leerArchivo canal =
            let
                fun loop acc =
                    case TextIO.inputLine canal of
                        SOME linea => 
                            let
                                val entrada_limpia = leerEntrada linea
                                val registroOpt = procesarLinea entrada_limpia
                            in
                                case registroOpt of
                                    SOME registro => loop (registro :: acc)
                                  | NONE => loop acc
                            end
                      | NONE => acc
            in
                loop []
            end
    in
        case TextIO.openIn ruta of
            canal =>
                let
                    val registros = leerArchivo canal
                in
                    TextIO.closeIn canal;
                    registros
                end
    end

(* Función para verificar si una cadena contiene caracteres no numéricos *)
fun contieneNoNumerico str =
    let
        (* Convertir la cadena en una lista de caracteres *)
        val chars = String.explode str
        (* Verificar si alguno de los caracteres no es un dígito *)
        val hayNoNumerico = List.exists (fn c => not (Char.isDigit c)) chars
    in
        hayNoNumerico
    end

(* Función para validar que la cuenta tenga exactamente 4 dígitos numéricos *)
fun validarCuenta cuenta =
    let
        (* Verificar la longitud y si es numérica *)
        val esValida = String.size cuenta = 4 andalso not (contieneNoNumerico cuenta)
    in
        if esValida then
            cuenta
        else
            let
                val _ = TextIO.output (TextIO.stdOut, "Número de cuenta inválido. Debe tener exactamente 4 dígitos numéricos.\n")
                val _ = TextIO.flushOut TextIO.stdOut
            in
                validarCuenta (pedirEntrada "Número de cuenta: ")
            end
    end

(* Función auxiliar para verificar si una cadena representa un número *)
fun esNumero str =
    case Int.fromString str of
        SOME _ => true
      | NONE   => false

(* Función para validar fecha en el formato YYYY-MM-DD *)
fun validarFecha fecha =
    let
        val partes = String.fields (fn c => c = #"-") fecha
        (* Verificar que la fecha tenga exactamente 3 partes: año, mes, día *)
        val esFormatoValido = 
            length partes = 3 andalso
            List.all (fn p => esNumero p) partes
    in
        case esFormatoValido of
            false => false
          | true =>
              let
                  val [anoStr, mesStr, diaStr] = partes
                  val ano = Int.fromString anoStr
                  val mes = Int.fromString mesStr
                  val dia = Int.fromString diaStr
              in
                  (* Verificar rangos válidos *)
                  case (ano, mes, dia) of
                      (SOME anoVal, SOME mesVal, SOME diaVal) =>
                          anoVal >= 1000 andalso anoVal <= 9999 andalso
                          mesVal >= 1 andalso mesVal <= 12 andalso
                          diaVal >= 1 andalso diaVal <= 31 
                    | _ => false
              end
    end

(* Función para validar hora en el formato HH:MM:SS *)
fun validarHora hora =
    let
        val partes = String.fields (fn c => c = #":") hora
        (* Verificar que la hora tenga exactamente 3 partes: hora, minuto, segundo *)
        val esFormatoValido = 
            length partes = 3 andalso
            List.all (fn p => esNumero p) partes
    in
        case esFormatoValido of
            false => false
          | true =>
              let
                  val [horaStr, minutoStr, segundoStr] = partes
                  val hora = Int.fromString horaStr
                  val minuto = Int.fromString minutoStr
                  val segundo = Int.fromString segundoStr
              in
                  (* Verificar rangos válidos *)
                  case (hora, minuto, segundo) of
                      (SOME horaVal, SOME minutoVal, SOME segundoVal) =>
                          horaVal >= 0 andalso horaVal < 24 andalso
                          minutoVal >= 0 andalso minutoVal < 60 andalso
                          segundoVal >= 0 andalso segundoVal < 60
                    | _ => false
              end
    end

(* Función para validar la fecha y hora en el formato YYYY-MM-DD HH:MM:SS *)
fun validarFechaHora fechaHora =
    let
        val partes = String.fields (fn c => c = #" ") fechaHora
        (* Verificar que la fechaHora tenga exactamente 2 partes: fecha y hora *)
        val esFormatoValido = 
            length partes = 2
    in
        case esFormatoValido of
            false => false
          | true =>
              let
                  val [fecha, hora] = partes
              in
                  validarFecha fecha andalso validarHora hora
              end
    end

(* Función para validar la fecha y hora *)
fun validarFechaHoraEntrada entrada =
    if validarFechaHora entrada then
        entrada
    else
        let
            val _ = TextIO.output (TextIO.stdOut, "Fecha y hora inválidas. Deben seguir el formato YYYY-MM-DD HH:MM:SS.\n")
            val _ = TextIO.flushOut TextIO.stdOut
        in
            validarFechaHoraEntrada (pedirEntrada "Fecha y hora (YYYY-MM-DD HH:MM:SS): ")
        end
  
(* Función para validar el tipo de transacción *)
fun validarTipoTransaccion opcion =
    case opcion of
        "1" => "deposito"
      | "2" => "retiro"
      | "3" => "transferencia"
      | _   =>
          let
              val _ = TextIO.output (TextIO.stdOut, "\nOpción inválida. Debe ingresar 1, 2 o 3.\n")
              val _ = TextIO.flushOut TextIO.stdOut
          in
              validarTipoTransaccion (pedirEntrada "\nSeleccione el tipo de transacción (1. Deposito, 2. Retiro, 3. Transferencia): ")
          end

(* Función para verificar si una cadena representa un número positivo *)
fun esNumeroPositivo str =
    let
        (* Función auxiliar para intentar convertir una cadena a un número *)
        fun esNumero str =
            case Real.fromString str of
                SOME _ => true
              | NONE   => false
        
        (* Verificar si el número es positivo *)
        val numeroValido = 
            let
                val realOpt = Real.fromString str
            in
                case realOpt of
                    SOME n => n > 0.0
                  | NONE   => false
            end
    in
        numeroValido
    end

(* Función para validar el monto *)
fun validarMonto monto =
    if esNumeroPositivo monto then
        monto
    else
        let
            val _ = TextIO.output (TextIO.stdOut, "Monto inválido. Debe ser un número positivo (entero o flotante).\n")
            val _ = TextIO.flushOut TextIO.stdOut
        in
            validarMonto (pedirEntrada "Monto: ")
        end


