#r @"D:\projects\PDF\PDFSharp\PdfSharp.dll"
open PdfSharp.Pdf
open PdfSharp.Pdf.IO
open PdfSharp.Pdf.Content
open PdfSharp.Pdf.Content.Objects

open System.Text

let directory = @"D:\projects\PDF\"
let inputfile = directory + @"wadler87"+".pdf"
let outputfile = inputfile + ".txt"
let rawPDF = PdfReader.Open(inputfile)

/// extract all text render operations of all pages,
/// where the desired payload text is scattered 
/// across the arguments of the "Tj" operations
let rawTextEncoding (encoder: Encoding) separator (rawPDF: PdfDocument) =
    [ for page in rawPDF.Pages do 
            let pageElements = if page = null then null else page.Elements
            let pageContent = if pageElements = null then null else pageElements.GetDictionary(@"/Contents")
            let pageTextStream = if pageContent = null then null else pageContent.Stream.UnfilteredValue
            match pageTextStream with
            | null -> ignore pageTextStream 
            | _ -> yield (encoder.GetString pageTextStream).Split separator  ]

/// standard ASCII encoder
let rawTextASCII separator (rawPDF: PdfDocument) =
    let encoder = new System.Text.ASCIIEncoding()
    rawTextEncoding encoder separator rawPDF

/// UTF7 encoder treats German special characters correctly 
let rawTextUTF7 separator (rawPDF: PdfDocument) =
    let encoder = new System.Text.UTF7Encoding()
    rawTextEncoding encoder separator rawPDF

/// save the raw text to disc for further analysis
let printPDFToFile (rawToText: PdfDocument -> string [] list) rawPDF (filename: string)  = 
    use outStream = new System.IO.StreamWriter(filename)
    rawPDF |> rawToText |> List.map System.String.Concat |> List.map outStream.WriteLine |> ignore
    outStream.Close()

printPDFToFile (rawTextASCII [|'\r'|]) rawPDF outputfile
    
/// extract the text blocks identified by PDF text markers 'BT' and 'ET',
/// keep the order of text blocks as derived from the PDF format,
/// add text block index and keep the positioning information per text block
let indexedTextBlocks rawText = 
    [ for r in rawText do 
        let (_,_,list) = r |> Array.fold (fun (flag,lastIndex,list) item -> 
                        match (flag,lastIndex,item) with
                        | (false,_,"BT") -> (true,(lastIndex+1),list)
                        | (true,_,"ET")  -> (false,lastIndex,list)
                        | (true,_,_)     -> (true,lastIndex,(list@[(lastIndex,item)]))
                        | (false,_,_)    -> (false,lastIndex,list) ) (false,-1,[]) 
        yield list ]
                  
rawTextASCII [|'\r'|] rawPDF |> indexedTextBlocks

open System.Text.RegularExpressions

/// http://stackoverflow.com/questions/5684014/f-mapping-regular-expression-matches-with-active-patterns
///Match the pattern using a cached compiled Regex
let (|CompiledMatch|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern, RegexOptions.Compiled)
        if m.Success then Some [for x in m.Groups -> x]
        else None

/// extract text 'Tj' method call parameter list
let textOfTjCall (Tj: string) =
    let test1 = (|CompiledMatch|_|) @"\((?<text>.*)\)\s*T[j|J]" Tj
    let test2 = (|CompiledMatch|_|) @"\[(?<text>.*)\]\s*T[j|J]" Tj
    let res1 =    
        match test1 with
        | Some [x;y] -> (y.Value).ToCharArray()
        | _ as x -> [||]
    let res2 =    
        match test2 with
        | Some [x;y] -> (y.Value).ToCharArray()
        | _ as x -> [||]
    res1.ToString() //+ res2

let Tj1 = "(emphasize this )Tj"
let Tj2 = "[(Not)-3(e)-3(: )-3(T)11(h)-3(i)-4(s)-6( )11(d)-3(o)-3(c)7(u)-3(m)6(e)-3(n)-3(ta)-5(t)11(i)-4(o)-3(n)-3( )11(i)-4(s)-6( )11(th)7(e)-3( )] TJ"

let x1 = textOfTjCall Tj1
let x2 = textOfTjCall Tj2
let y =  new string([|'a'; 'b'|])