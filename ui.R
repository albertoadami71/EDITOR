shinyUI(fluidPage(
  titlePanel("Message Editor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("1. Write your message in the text area;"),
      helpText("2. Press the button 'next word' to view a list of words and select a suggestion;"),
      helpText("3. Please, type slowly and be patient."),
      
      
     tags$button("next word", class="btn btn-default action-button", id = "botao", type="button", 
                    onclick="var a =document.getElementById('text'); var res = getCaret(a);
                    var b= document.getElementById('contador');b.value=''+res;var d = document.getElementById('selector').size='4';"), 
   tags$div(tags$select(size = '1', tabIndex='-1', style ='width: 250px', class='selectized shiny-bound-input', id='selector', 
                                              tags$option(' ', value='') )  )

      ),
    
    mainPanel(


    tags$textarea(id="text", type = "text", value = "texto", rows = "4", cols="50",onchange = 
                                    HTML("var a =document.getElementById('text'); var e =document.getElementById('text6');
                                     var res = getCaret(a);  
                                    var todo = a.value; var texto = todo.substr(0,res);var resto = todo.substr(res);                                    var tam = texto.length-1; var tamorig = tam;
                                    if (texto.length <=1) {texto = texto.toUpperCase()} else {
                                    if (texto.charAt(tam)!=' ') {tam = tam-1;
                                        if (texto.charAt(tam)==' '){
                                            while((texto.charAt(tam)==' ')&&(tam>=0)){tam=tam-1;} 
                                            if (tam>=0) {var caraca = texto.charAt(tam);
                                             if ((caraca=='.')||(caraca=='!')||(caraca=='?')) {
                                                var ultima = texto.substr(tamorig);
                                                texto = texto.substr(0,tamorig)+ultima.toUpperCase();
                                             };                                            
                                            };    
                                        };
                                    };
                                    }
                                    
                                    a.value = texto+resto;
                                  setCaretPosition('text', res); todo=a.value; var nchars = todo.length; e.value=''+nchars;
                                  var b= document.getElementById('contador');var d= document.getElementById('selector');d.size='1';
                                    b.value=''+res;
                                       "),  
                                  onkeypress = "this.onchange();", oninput = "this.onchange();", onkeyup="this.onchange();"),
      textOutput("text2", container=div),
      textOutput("text3", container=div),
      textOutput("text4", container=div),
      textOutput("text5", container=div),
 
     tags$script(HTML("function getCaret(el) { 
  if (el.selectionStart) { 
                       return el.selectionStart; 
                       } else if (document.selection) { 
                       el.focus(); 
                       
                       var r = document.selection.createRange(); 
                       if (r == null) { 
                       return 0; 
                       } 
                       
                       var re = el.createTextRange(), 
                       rc = re.duplicate(); 
                       re.moveToBookmark(r.getBookmark()); 
                       rc.setEndPoint('EndToStart', re); 
                       
                       return rc.text.length; 
                       }  
                       return 0; 
                       }")),


tags$script(HTML("
                 
                 function setCaretPosition(elemId, caretPos) {
    var elem = document.getElementById(elemId);
                 
                 if(elem != null) {
                 if(elem.createTextRange) {
                 var range = elem.createTextRange();
                 range.move('character', caretPos);
                 range.select();
                 }
                 else {
                 if(elem.selectionStart) {
                 elem.focus();
                 elem.setSelectionRange(caretPos, caretPos);
                 }
                 else
                 elem.focus();
                 }
                 }
                 }
                 
                 ")),

tags$input(id='text6', type = "text", size="5"),    

tags$input(id='contador', type = "text", style="visibility: hidden",
               onchange="var a =document.getElementById('text');a.focus();", onfocus="this.onchange();")

      
    )
  )
))