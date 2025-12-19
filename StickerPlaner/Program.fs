open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

let konst x _ = x

let inline scan (im: Image<Rgba32>) outer inner ([<InlineIfLambda>] toXY) ([<InlineIfLambda>] pick) =
    seq {
        for a in outer do
            for b in inner do
                let x, y = toXY a b
                if im[x, y].A <> 0uy then yield pick a b
    }
    |> Seq.tryHead
    |> Option.defaultValue 0

/// The bounding box contains the row and column indexes for the first occupied pixels in each direction. Therefore, it
/// is important to retain these coordinates when cropping.
type BoundingBox =
    { Width: int
      Height: int
      Left: int
      Right: int
      Top: int
      Bottom: int }
    static member Create(im: Image<Rgba32>) =
        let width, height = im.Width, im.Height

        let xy = fun x y -> x, y
        let yx = fun y x -> x, y

        let left = scan im (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 }) xy konst

        let right =
            scan im (seq { width - 1 .. -1 .. 0 }) (seq { 0 .. height - 1 }) xy konst

        let top = scan im (seq { 0 .. height - 1 }) (seq { 0 .. width - 1 }) yx konst

        let bottom =
            scan im (seq { height - 1 .. -1 .. 0 }) (seq { width - 1 .. -1 .. 0 }) yx konst

        { Width = width
          Height = height
          Left = left
          Right = right
          Top = top
          Bottom = bottom }

    member this.HorizontalMargin() = this.Width - this.Right + this.Left - 1
    member this.VerticalMargin() = this.Width - this.Bottom + this.Top - 1

    member this.CropBoundingBox dar =
        // If the aspect ratio is square, we don't need to do anything
        // If the aspect ratio is wide, we need to crop vertically to match the horizontal margin
        // If the aspect ratio is tall, we need to crop horizontally to match the vertical margin
        // However, we may have added extra margin on one side that goes out of bounds
        // Therefore, we first need to expand the image canvas to accommodate the new bounding box
        // Then, we can crop to the new bounding box
        match dar with
        | Square -> this // TODO: make sure that we don't change the image aspect ratio here. We may need to pad/crop to a square.
        | Wide ->
            let dy = this.HorizontalMargin() / 2

            // Since the padding will expand the canvas in both directions, the new top index should be
            // top - dy + dy = top. Similarly, the new bottom index should be bottom + dy + dy = bottom + 2 * dy.
            { this with
                Height = this.Height + dy * 2
                Bottom = this.Bottom + dy * 2
                Left = 0
                Right = this.Width - 1 }
        | Tall ->
            let dx = this.VerticalMargin() / 2

            // Ditto
            { this with
                Width = this.Width + dx * 2
                Left = this.Left
                Right = this.Right + dx * 2
                Top = 0
                Bottom = this.Height - 1 }

    member this.AsRectangle() =
        Rectangle(this.Left, this.Top, this.Right - this.Left + 1, this.Bottom - this.Top + 1)

and DesiredAspectRatio =
    | Square
    | Wide
    | Tall
    static member Create(bbox: BoundingBox) =
        let h = double <| bbox.HorizontalMargin()
        let v = double <| bbox.VerticalMargin()
        // If vertical margin is much greater than horizontal, then wide
        // If horizontal margin is much greater than vertical, then wide
        // Otherwise, square
        if v * 1.1 > h then Wide
        elif h * 1.1 > v then Tall
        else Square

    member this.ResizeWidth() =
        match this with
        | Square -> 512
        | Wide -> 512
        | Tall -> 0

    member this.ResizeHeight() =
        match this with
        | Square -> 512
        | Wide -> 0
        | Tall -> 512

let processFile (inputPath: string) (outputPath: string) =
    let im = Image.Load<Rgba32> inputPath

    let bb = BoundingBox.Create im
    let dar = DesiredAspectRatio.Create bb
    let cropBoundingBox = bb.CropBoundingBox dar

    im.Mutate (fun ctx ->
        ctx
            .Pad(cropBoundingBox.Width, cropBoundingBox.Height)
            .Crop(cropBoundingBox.AsRectangle())
            .Resize(dar.ResizeWidth(), dar.ResizeHeight())
        |> ignore<IImageProcessingContext>)

    im.Save outputPath

[<EntryPoint>]
let main argv =
    argv
    |> Array.iter (fun path ->
        let outputPath =
            Path.Combine(Path.GetDirectoryName(path), "StickerPlaner", Path.GetFileName(path))

        processFile path outputPath)

    0
