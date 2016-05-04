// module FileReader


exports.readFileImpl = function readText(file, onSuccess, onFailure) {
  if (file) {
      var reader = new FileReader();
      reader.readAsText(file, "UTF-8");
      reader.onload = onSuccess
      reader.onerror = onFailure
      }
}
exports.readFileBlocking = function getFileBlocking(filename) { 
  var xmlhttp = new XMLHttpRequest(); xmlhttp.responseType = "text";
  xmlhttp.responseType = "text"
  xmlhttp.open("GET", filename, true);
  xmlhttp.send();
  return xmlhttp.responseText
}
