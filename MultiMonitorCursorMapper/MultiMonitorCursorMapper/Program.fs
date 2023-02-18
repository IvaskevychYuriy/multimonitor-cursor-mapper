open System.Windows.Forms
open System
open System.Threading

[<Measure>] type px
[<Measure>] type mm
[<Measure>] type inch

let inchesToMm (x: float<inch>) =
    x * 25.4<mm/inch>

[<Struct>] type Point = { X: int<px>; Y: int<px> }
[<Struct>] type VerticalLine = { P0: Point; Y1: int<px> }
[<Struct>] type CrossingDirection = | FromLeft | FromRight

[<Struct>] type Portal = { Line: VerticalLine; CrossingDirection: CrossingDirection; RealLength: float<mm>; Scale: float }

let mapToPoint (p: Drawing.Point) =
    { X = p.X * 1<_>; Y = p.Y * 1<_>}

let mapFromPoint p =
    new Drawing.Point(int p.X, int p.Y)

let directionOrNone pCurr pPrev =
    if pCurr.X < pPrev.X then Some FromRight
    elif pCurr.X > pPrev.X then Some FromLeft
    else None

let entersPortal point portal dir =
    int point.X = int (float (portal.Line.P0.X) / portal.Scale) && dir = portal.CrossingDirection

let len l =
    l.Y1 - l.P0.Y + 1<_>

let clamp a b x =
    x |> max a |> min b

let portalExitPoint point (enter, exit) =
    let relativeH =
        ((float enter.Line.Y1 / enter.Scale) - (float point.Y))
        |> (*) (enter.Scale / exit.Scale)
        |> (*) (enter.RealLength / exit.RealLength)
        |> (*) (float (len exit.Line) / float (len enter.Line))

    let y = clamp
                (float exit.Line.P0.Y / exit.Scale)
                (float exit.Line.Y1 / exit.Scale)
                (float exit.Line.Y1 / exit.Scale - relativeH)
    {
        X = (float (exit.Line.P0.X) / exit.Scale) |> int |> LanguagePrimitives.Int32WithMeasure;
        Y = y |> int |> LanguagePrimitives.Int32WithMeasure
    }

let monitorHeight (w: int<px>) (h: int<px>) (d: float<inch>) =
    let denominator = sqrt (float w ** 2 + float h ** 2)
    float h * d / denominator

let getCurrentDirection dirPrev pCurr pPrev =
    directionOrNone pCurr pPrev
    |> Option.orElse dirPrev

let tryFindExitPoint portals currDir pCurr pPrev =
    let portalPair =
        portals
        |> List.tryFind (fun ps ->
            currDir
            |> Option.map (entersPortal pCurr (fst ps))
            |> Option.defaultValue false)
    let exitPoint = portalPair |> Option.map (portalExitPoint pCurr)
    if exitPoint.IsSome then printfn "portalPair: %A" portalPair; printfn "curr: %A prev: %A dir: %A exitPoint: %A" pCurr pPrev currDir exitPoint

    exitPoint

let getCursorPos() = mapToPoint Cursor.Position

let setCursorPos p =
    printfn "moving to %A" p
    let point = mapFromPoint p
    Cursor.Position <- point

let offset dir =
    match dir with
    | FromRight -> -2<px>
    | FromLeft -> +2<px>

[<EntryPoint>]
let main args =
    let leftMonitorHeightMm = 23.0<inch> |> monitorHeight 1920<px> 1080<px> |> inchesToMm
    let mainMonitorHeightMm = 27.0<inch> |> monitorHeight 2560<px> 1440<px> |> inchesToMm
    let rightMonitorHeightMm = 29.0<inch> |> monitorHeight 2560<px> 1080<px> |> inchesToMm

    let portalsTwoWay = [
        (   { Line = { P0 = { X = -1<px>; Y = 350<px> }; Y1 = 1429<px> }; CrossingDirection = FromLeft; RealLength = leftMonitorHeightMm; Scale = 1.0 },
            { Line = { P0 = { X = 0<px>; Y = 0<px> }; Y1 = 1439<px> }; CrossingDirection = FromRight; RealLength = mainMonitorHeightMm; Scale = 1.25 });
        (   { Line = { P0 = { X = 2559<px>; Y = 0<px> }; Y1 = 1439<px> }; CrossingDirection = FromLeft; RealLength = mainMonitorHeightMm; Scale = 1.25 },
            { Line = { P0 = { X = 2560<px>; Y = 347<px> }; Y1 = 1426<px> }; CrossingDirection = FromRight; RealLength = rightMonitorHeightMm; Scale = 1.0 });
    ]

    let portals =
        portalsTwoWay
        |> List.collect (fun (p1, p2) -> [(p1, p2); (p2, p1)])

    let mutable p1 = getCursorPos()
    let mutable dir1 = None
    Thread.Sleep(5);

    while true do
        let p0 = getCursorPos()
        let dir0 = getCurrentDirection dir1 p0 p1
        let exitPoint = tryFindExitPoint portals dir0 p0 p1
        Option.map setCursorPos exitPoint |> ignore

        p1 <- p0
        dir1 <- dir0
        Thread.Sleep(3);

    Console.Read() |> ignore
    0