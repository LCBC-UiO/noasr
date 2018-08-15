output$HelpPage = renderUI({
  fluidPage(
    fluidRow(HTML("<br>")),
    fluidRow(
      column(1),
      column(10,wellPanel(
        h3("'Search'"),
        HTML("<div align='left'>Allows you to search for observations matching certain characteristics
             (like Age above 30: <i>Age>30</i>). Search is case-sensitive, meaning capital and small letters must be correct.<br> <br>
             You can create complex searches to subset the data, but each expression must consist of
             [Column_name Expression Value] (search for empty or not empty values are the only expetion to this, see below).
             Different search parameters can be specified with OR-sign (|) and AND-sign (&)
             (like 'Age >= 20 | Age <= 50' to get all between the ages of 20 and 50)<br><br>"),
        HTML("<p><h4>Between queries</h4>
             <table width='60%'>
             <tr> <th>Expression</th> <th align='center'>Meaning</th> </tr>
             <tr> <td>x & y</td> <td>x and y</td> </tr>
             <tr> <td>x | y</td> <td>x or y</td> </tr>
             </table><br><br>
             <figure align='center' style='color: #626262; font-size: 10px;'>
             <img src='Logicals.png' class='img-responsive' align='center' width='100%' height='auto' style='max-width:600px;'/>
             <figcaption>image from http://r4ds.had.co.nz/transform.html</figcaption>
             </figure>"),
        HTML("<br><br>
             <p><h4>Query expression</h4>
             <table width='100%'>
             <tr> <th>Expression</th> <th align='center'>Meaning</th> </tr>
             <tr> <td>x == y</td> <td>x equal to y </tr>
             <tr> <td>x != y</td> <td>x not equal to y</td> </tr>
             <tr> <td>x > y</td> <td>x larger than y</td> </tr>
             <tr> <td>x >= y</td> <td>x larger or equal to y</td> </tr>
             <tr> <td>x < y</td> <td>x smaller than y</td> </tr>
             <tr> <td>is.na(x)</td> <td>x has no value (NA=not applicable/not a number) </td> </tr>
             <tr> <td>![expr]</td> <td>Negation (will inverts statements above, like '!=' to '==')</td> </tr>
             </table><br><br></p>"),
        HTML("<p><h4>Examples:</h4>
             <ul>
             <li><b>Age >= 20 - </b> all observations with Age above or equal to 20.<br></li>
             <li><b>!is.na(CVLT_A_Total) - </b>gives all observations where there IS a CVLT_A_Total value <br></li>
             <li><b>Age >= 20 & !is.na(CVLT_A_Total) - </b> all observations that have Age over or equal to 20 AND have CVLT_A_Total scores. <br></li>
             <li><b>Age >= 20 | !is.na(CVLT_A_Total) - </b> all observations that have either Age over or equal to 20 or have CVLT_A_Total scores. <br></li>
             <li><b>Age >= 20 | Age <= 50' - </b>all observations between the ages of 20 and 50.<br></li>
             <li><b>(Age<10 & WISC_Digitspan_FWD > 10) | (Age>20 & WASI_DigitSpan_FWD > 20) - </b></li>
             complex search. all observations where those aged below 10 must have above 12 in digit span,
             and adults aobve 20 who have more than 20 on the DigitSpan.</li>
             </ul></p></div>")
        )))
        )
})