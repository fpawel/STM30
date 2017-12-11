#I "../packages"
#I "../packages/FParsec.1.0.2/lib/net40-client/"
#r "FParsec.dll"
#r "FParsecCS.dll"

#I "bin/release"

#r @"Widgets.dll"
#r @"MyWinForms.dll"
#r @"System.Drawing.dll"
#r @"System.Windows.Forms.dll"
#r @"System.ComponentModel.DataAnnotations"

#load "Utils\Utils.fs"
#load "Utils\Runtime.fs"
#load "Utils\Bin.fs"
#load "Utils\Json\Json.fs"
#load "Utils\Json\JsonSerialization.fs"
#load "Utils\Json\JsonConfig.fs"
#load "Utils\WinForms\MyWinFormsConvs.fs"
#load "AppCore\Logging.fs"
#load "AppCore\ComportConfig.fs"
#load "AppCore\Comport.fs"
#load "AppCore\AppConfig.fs"
#load "AppCore\Mdbs.fs"
#load "AppCore\Stend.fs"

#load "STM30\Mdbs16.fs"
#load "STM30\STM30.fs"

let cfg = AppConfig.config
cfg.ComportProducts.PortName <- "COM1"
cfg.ComportStend.PortName <- "COM2"
cfg.ComportProducts.CanLog <- true

Mdbs16.write 1uy Mdbs16.SET_U_0 0m

let (~%%) n = 
    Mdbs.write 1uy (Mdbs16.INC_U_0.Code + 256 * (abs n)) "подстройка" (if n>0 then 0m else 1m)
    Stend.getu 1uy

Mdbs16.write 1uy Mdbs16.SET_U_0 1m

Comport.Ports.closeOpenedPorts( fun _ _ -> true)