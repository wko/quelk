var toggler = document.getElementsByClassName("caret");
var i;

for (i = 0; i < toggler.length; i++) {
  toggler[i].addEventListener("click", function() {
    this.parentElement.querySelector(".nested").classList.toggle("active");
    this.classList.toggle("caret-down");
  });
}

filterSelection("all")

function findAncestor (el, cls) {
    while ((el = el.parentElement) && !el.classList.contains(cls));
    return el;
}

function filterSelection(c) {
  
  var x, i;
  x = document.getElementsByClassName("filterDiv");
  if (c == "all") {
    for (i = 0; i < x.length; i++) {
      x[i].classList.add("active");
    }
  } else {
    //console.log("Filtering for " + c)
    for (i = 0; i < x.length; i++) {
      x[i].classList.remove("active");
      if (x[i].getAttribute('data') == c) x[i].classList.add("active");
    }
  }
  // Add the "active" class (display:block) to the filtered elements, and remove the "active" class from the elements that are not selected
  updateBadges();
}


function updateBadges() {
  var i;
  var x = document.getElementsByClassName("badge");
  for (i = 0; i < x.length; i++) {
    var l = x[i].closest("li")
    var a = l.querySelector("ul.answers").children
    var j;
    var c = 0;
    for (j=0; j< a.length; j++) {
      if (a[j].firstElementChild.classList.contains("active")) {
        c++;
      }
    }
    x[i].textContent = c;
  }
}

// Add active class to the current control button (highlight it)
/*var btnContainer = document.getElementById("myBtnContainer");
var btns = btnContainer.getElementsByClassName("btn");
for (var i = 0; i < btns.length; i++) {
  btns[i].addEventListener("click", function() {
    var current = document.getElementsByClassName("active");
    current[0].className = current[0].className.replace(" active", "");
    this.className += " active";
  });
}*/
