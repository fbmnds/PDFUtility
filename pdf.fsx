#r @"D:\projects\PDF\PDFSharp\PdfSharp.dll"
open PdfSharp.Pdf
open PdfSharp.Pdf.IO
open PdfSharp.Pdf.Content
open PdfSharp.Pdf.Content.Objects

open System.Text

let directory = @"D:\projects\PDF\"
let inputfile = directory + @"Fsharp_Succinctly"+".pdf"
let outputfileRaw = inputfile + ".raw.txt"
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

/// UTF8 encoder 
let rawTextUTF8 separator (rawPDF: PdfDocument) =
    let encoder = new System.Text.UTF7Encoding()
    rawTextEncoding encoder separator rawPDF

/// save the raw text to disc for further analysis
let printRawPDFToFile (rawToText: PdfDocument -> string [] list) rawPDF (filename: string)  = 
    use outStream = new System.IO.StreamWriter(filename)
    rawPDF |> rawToText |> List.map System.String.Concat |> List.map outStream.WriteLine |> ignore
    outStream.Close()

printRawPDFToFile (rawTextUTF8 [|'\r'|]) rawPDF outputfileRaw
    
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
                  
let textBlocks = rawTextUTF7 [|'\r';'\n'|] rawPDF |> indexedTextBlocks

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
/// (write parse error message to stderr)
let textOfTjCall (Tj: string) =
    let test1 = (|CompiledMatch|_|) @"\((?<text>.*)\)\s*T[j|J]" Tj
    let res1 =    
        match test1 with
        | Some [x;y] -> (y.Value)
        | _ as x -> ""
    if res1 <> "" then res1
    else 
        let test2 = (|CompiledMatch|_|) @"\n*\r*\s*\[(?<text>.*)\]\s*T[j|J]" Tj
        let res2 =    
            match test2 with
            | Some [x;y] -> (y.Value).ToCharArray()
            | _ as x -> [||]
        let (_,_,x) = res2 
                      |> Array.fold 
                            (fun (read,escaped,acc) c -> 
                                match (read,escaped,c) with
                                | (false,false,'(') -> (true,false,acc)
                                | (true,false,')') -> (false,false,acc)
                                /// handle escaped paranthesis
                                /// TODO: necessary to handle escaped brackets?
                                | (true,true,'(') -> (true,false,acc+"(")
                                | (true,true,')') -> (true,false,acc+")")
                                /// other escaped characters are kept escaped
                                | (true,true,_) -> (true,false,acc+"\\"+string(c))
                                /// handle ecape mode
                                | (true,false,'\\') -> (true,true,acc)
                                /// read other characters
                                | (true,false,_) -> (true,false, acc + string(c)) 
                                | (false,false,_) -> (false,false,acc)
                                /// never escape while not reading
                                | (false,true,_) -> 
                                    fprintf stderr "Error: ignored parse error at position %d" acc.Length;(false,false,acc)) 
                            (false,false,"")
                                
        x

let Tj1 = "(emphasize this )Tj"
let Tj2 = "[(W)-22(h)13(a)13(t)6( )-4(I)-4(s)11( )-4(Fu)4(nc)13(t)-4(i)5(on)3(al)6( )-4(P)4(r)-3(o)13(g)-8(r)7(amm)-5(i)5(n)13(g)-8(?)] TJ"

let x1 = textOfTjCall Tj1
let x2 = textOfTjCall Tj2

let textOfPage (indexedPageBlocks: (int*string) list) =
    let (_,_,x) = 
        indexedPageBlocks
        |> List.fold 
            (fun (rowno,row,list) (blockno,item) -> 
                 if blockno = -1 then (0,(textOfTjCall item),[])
                 else if blockno = rowno then (rowno,row+(textOfTjCall item),list)
                 else (blockno,(textOfTjCall item),list@[row]))
            (-1,"",[])
    x

let textOfDocument (indexedDocBlocks: (int*string) list list) =
    indexedDocBlocks
    |> List.map textOfPage

let x3 = textBlocks |> textOfDocument

/// save the PDF text to file
let printPDFToFile (rawToText: PdfDocument -> string [] list) rawPDF (filename: string)  = 
    use outStream = new System.IO.StreamWriter(filename)
    rawTextUTF7 [|'\r';'\n'|] rawPDF |> indexedTextBlocks |> textOfDocument 
    |> List.map System.String.Concat |> List.map outStream.WriteLine |> ignore
    outStream.Close()
    
printPDFToFile (rawTextUTF8 [|'\r';'\n'|]) rawPDF outputfile

