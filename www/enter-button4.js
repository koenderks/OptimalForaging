$(document).keyup(function(event) {
    if ($("#word4").is(":focus") && (event.keyCode == 13)) {
        $("#submit4").click();
        $("#word4").val("");
    }
});

$(document).ready(function() {

$("#submit4").on("click", function() {
    
    $("#word4").val("");
    
});

});