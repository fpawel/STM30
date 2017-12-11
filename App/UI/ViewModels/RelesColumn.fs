namespace MyWinForms

open System
open System.Drawing
open System.Windows.Forms
open System.Windows.Forms.VisualStyles
open System.ComponentModel


type ReleStateInfo = Result<string, Set<STM30.Rele>> option

type DataGridViewRelesCell() =
    inherit DataGridViewTextBoxCell()
    override this.Paint(    graphics,
                            clipBounds,
                            cellBounds,
                            rowIndex,
                            cellState,
                            value,
                            formattedValue,
                            errorText,
                            cellStyle,
                            advancedBorderStyle,
                            paintParts) = 
        
        match (value :?> STM30.ViewModels.Reader).Rele with
        | Some ( Ok v ) ->
            let st0 = enum<DataGridViewElementStates>(0)

            // Clear cell             
            let brs = 
                if (cellState &&& DataGridViewElementStates.Selected) <> st0 then
                    SystemBrushes.Highlight
                else
                    new SolidBrush(cellStyle.BackColor) :> Brush
            graphics.FillRectangle(brs, cellBounds)

            
            
            let gridBrush = new SolidBrush( base.DataGridView.GridColor )
            let pen = new Pen(gridBrush)
            //Bottom line drawing
            graphics.DrawLine(pen, cellBounds.Left, cellBounds.Bottom-1 , cellBounds.Right, cellBounds.Bottom-1)
            //Right line drawing
            graphics.DrawLine(pen, cellBounds.Right-1, cellBounds.Top , cellBounds.Right-1, cellBounds.Bottom-1)

            let flags = TextFormatFlags.VerticalCenter ||| TextFormatFlags.Left ||| TextFormatFlags.EndEllipsis
            let font = cellStyle.Font
            STM30.Rele.values
            |> List.filter( fun x -> Set.contains x v)
            |> List.map STM30.Rele.info
            |> List.iter(fun (_, text, foreColor, _) -> 
                let offset = TextRenderer.MeasureText(text, cellStyle.Font)
                TextRenderer.DrawText(graphics, text, cellStyle.Font, cellBounds, foreColor, flags)                     
                cellBounds.Offset(offset.Width + 2, 0) )
        | _ ->
            // Call the base class method to paint the default cell appearance.
            base.Paint(graphics, clipBounds, cellBounds, rowIndex, cellState,
                value, formattedValue, errorText, cellStyle,
                advancedBorderStyle, paintParts)

    // Force the cell to repaint itself when the mouse pointer enters it.
    override this.OnMouseEnter(rowIndex) =    
        this.DataGridView.InvalidateCell(this)
    

    // Force the cell to repaint itself when the mouse pointer leaves it.
    override this.OnMouseLeave(rowIndex) =
        this.DataGridView.InvalidateCell(this)

type DataGridViewRelesColumn() =
    inherit DataGridViewColumn()
    do
        base.CellTemplate <- new DataGridViewRelesCell()
        
