
function myFunction() {
  var x = document.getElementsByClassName("Code");
  if (x[0].style.display === "none") {
    x[0].style.display = "block";
  } else {
    x[0].style.display = "none";
  }
}


var slideIndex = 1;
showDivs(slideIndex);

function plusDivs(n) {
  showDivs(slideIndex += n);
}

function showDivs(n) {
  var i;
  var x = document.getElementsByClassName("plots");
  if (n > x.length) {slideIndex = 1}
  if (n < 1) {slideIndex = x.length}
  for (i = 0; i < x.length; i++) {
    x[i].style.display = "none";  
  }
  x[slideIndex-1].style.display = "block";  
}


/*
New buttons: Slice

*/
function plusDivs2(n) {
  showDivs2(slideIndex += n);
}

function showDivs2(n) {
  var i;
  var x = document.getElementsByClassName("model_performance");
  if (n > x.length) {slideIndex = 1}
  if (n < 1) {slideIndex = x.length}
  for (i = 0; i < x.length; i++) {
    x[i].style.display = "none";  
  }
  x[slideIndex-1].style.display = "block";  
}



/*
New buttons: Slice

*/
function plusDivs3(n) {
  showDivs3(slideIndex += n);
}

function showDivs3(n) {
  var i;
  var x = document.getElementsByClassName("decision_tree");
  if (n > x.length) {slideIndex = 1}
  if (n < 1) {slideIndex = x.length}
  for (i = 0; i < x.length; i++) {
    x[i].style.display = "none";  
  }
  x[slideIndex-1].style.display = "block";  
}





