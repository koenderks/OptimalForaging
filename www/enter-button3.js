$(document).keyup(function(event) {
    if ($("#word3").is(":focus") && (event.keyCode == 13)) {
        $("#submit3").click();
        $("#word3").val("");
    }
});

$(document).ready(function() {

$("#submit3").on("click", function() {
    
    $("#word3").val("");
    
});

});