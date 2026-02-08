<script>
(function() {
  var divHTML = document.querySelectorAll(".hideoutput-open");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[1];
    outputNode.outerHTML = "<details open class='output'><summary>Output</summary>" + outputNode.outerHTML + "</details>";
  })
})();
(function() {
  var divHTML = document.querySelectorAll(".hideoutput");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[1];
    outputNode.outerHTML = "<details class='output'><summary>Output</summary>" + outputNode.outerHTML + "</details>";
  })
})();
(function() {
  var divHTML = document.querySelectorAll(".hidecode");
  divHTML.forEach(function (el) {
    var codeNode = el.getElementsByTagName("pre")[0]; // Assuming the first pre tag is the code
    codeNode.outerHTML = "<details><summary>Code</summary>" + codeNode.outerHTML + "</details>";
  })
})();
</script>