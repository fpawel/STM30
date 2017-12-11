module MyWinForms.Color

open System.Drawing

let fromString x = ColorTranslator.FromHtml(x)
let MidleAqua = fromString "#569CD6"
let MidleGreen = fromString "#84BB96"

    

