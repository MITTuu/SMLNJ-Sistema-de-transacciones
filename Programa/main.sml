use "auxiliares.sml";
use "creador.sml";
use "analizador.sml";

(* Función para manejar el menú del creador *)
fun menuCreador () =
    let
    	val archivoIndice = "Transacciones.csv"
    
        fun mostrarMenuCreador () =
            let
                val _ = TextIO.output (TextIO.stdOut, 
                    "\n _______________________________________\n" ^
                    "|                                       |\n" ^
                    "|          Gestor de índices            |\n" ^
                    "|_______________________________________|\n" ^
                    "1) Agregar un nuevo registro bancario\n" ^
                    "2) Limpiar registros\n" ^
                    "3) Salir\n" ^
                    "\nSeleccione una opción: ");
                val opcion = pedirEntrada ""
            in
                case opcion of
                    "1" => (agregarRegistro archivoIndice; mostrarMenuCreador ())
                  | "2" => (limpiarIndice archivoIndice; mostrarMenuCreador ())
                  | "3" => (TextIO.output (TextIO.stdOut, "\nSaliendo del programa...\n"); OS.Process.exit OS.Process.success)
                  | _   => (TextIO.output (TextIO.stdOut, "\nOpción inválida\n"); mostrarMenuCreador ())
            end
    in
        mostrarMenuCreador ()
    end;


(* Función para mostrar el menú del analizador *)
fun menuAnalizador () =
    let
        (* Solicitar la ruta del archivo *)
        val _ = TextIO.output (TextIO.stdOut, "Introduce la ruta del archivo a analizar (.csv): ")
        val _ = TextIO.flushOut TextIO.stdOut
        val ruta = pedirEntrada ""

        (* Leer y procesar el archivo *)
        val registros = leerArchivoCSV ruta 
		val registrosOrdenados = ordenarRegistrosPorMontoDescendente registros
		
        (* Función para mostrar el menú *)
        fun mostrarMenuAnalizador () =
            let
                val _ = TextIO.output (TextIO.stdOut, 
                    "\n _______________________________________\n" ^ 
                    "|                                       |\n" ^ 
                    "|     Analizador de transacciones       |\n" ^ 
                    "|_______________________________________|\n" ^ 
                    "1) Mostrar registros por rango de montos\n" ^ 
                    "2) Informe de actividades sospechosas\n" ^
                    "3) Transacciones por cuenta\n" ^
                    "4) Cantidad de transacciones por tipo\n" ^
                    "5) Resumen\n" ^
                    "6) Salir\n" ^ 
                    "\nSeleccione una opción: ")
                val _ = TextIO.flushOut TextIO.stdOut
                val opcion = pedirEntrada ""

                val _ = case opcion of
                    "1" =>
                        let                         
                            val _ = filtrarRegistrosPorMonto registrosOrdenados
                        in
                            mostrarMenuAnalizador ()
                        end
                        
                  | "2" => 
                  		let
                  			val _ = TextIO.output (TextIO.stdOut, "\nGenerando informe de actividades sospechosas...\n")
                  			val _ = filtrarTransaccionesSospechosas registros;
                  		in
                   		    mostrarMenuAnalizador ()
                  		end

                  | "3" => 
                  		let
                            val _ = TextIO.output (TextIO.stdOut, "\nIngrese el número de cuenta (4 digitos): ")
                            val _ = TextIO.flushOut TextIO.stdOut
                            val cuentaIngresada = validarCuenta (pedirEntrada "")
                            val cuentaConPrefijo = "ACC" ^ cuentaIngresada
                            val _ = filtrarTransaccionesPorCuenta registros cuentaConPrefijo
                  		in
                   		    mostrarMenuAnalizador ()
                  		end                

				  | "4" => 
				  		let
							(* Solicitar el tipo de transacción al usuario *)
							val _ = TextIO.output (TextIO.stdOut, "\nSeleccione el tipo de transacción (1. Deposito, 2. Retiro, 3. Transferencia): ")
							val _ = TextIO.flushOut TextIO.stdOut

							(* Obtener y validar el tipo de transacción ingresado *)
							val tipoIngresado = validarTipoTransaccion (pedirEntrada "")

							(* Contar las transacciones del tipo especificado *)
							val cantidadTransacciones = contarTransaccionesPorTipo registros tipoIngresado

							(* Mostrar la cantidad de transacciones *)
							val _ = TextIO.output (TextIO.stdOut, 
								"\nCantidad de transacciones de tipo " ^ tipoIngresado ^ ": " ^ 
								Int.toString cantidadTransacciones ^ "\n")
						in
							mostrarMenuAnalizador ()
						end
 
				  	   
				  | "5" =>
				  		let
							val _ = TextIO.output (TextIO.stdOut, "Generando resumen de transacciones...\n")
							val _ = resumen registrosOrdenados
						in
							mostrarMenuAnalizador ()
						end
                  
                  | "6" => (TextIO.output (TextIO.stdOut, "Saliendo del programa...\n"); OS.Process.exit OS.Process.success)
                  
                  | _   => (TextIO.output (TextIO.stdOut, "Opción no válida\n"); mostrarMenuAnalizador ())
            in
                mostrarMenuAnalizador ()
            end
    in
        mostrarMenuAnalizador ()
    end

(* Función principal *)
fun principal () =
    let
        fun mostrarMenuPrincipal () =
            let
                val _ = TextIO.output (TextIO.stdOut, 
                    "\n _______________________________________\n" ^
                    "|                                       |\n" ^
                    "|        Transacciones Bancarias        |\n" ^
                    "|_______________________________________|\n" ^
                    "1) Creador\n" ^
                    "2) Analizador\n" ^
                    "3) Salir\n" ^
                    "\nSeleccione una opción: ");
                val opcion = pedirEntrada ""
            in
                case opcion of
                    "1" => menuCreador ()
                  | "2" => menuAnalizador ()
                  | "3" => (TextIO.output (TextIO.stdOut, "\nSaliendo del programa...\n"); OS.Process.exit OS.Process.success)
                  | _   => (TextIO.output (TextIO.stdOut, "\nOpción inválida\n"); mostrarMenuPrincipal ())
            end
    in
        mostrarMenuPrincipal ()
    end;

(* Ejecutar la función principal *)
principal ();   
