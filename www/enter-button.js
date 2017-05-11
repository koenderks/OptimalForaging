$(document).keyup(function(event) {
    if ($("#word").is(":focus") && (event.keyCode == 13)) {
        $("#submit").click();
        $("#word").val("");
    }
});

$(document).ready(function() {

$("#submit").on("click", function() {
    
    $("#word").val("");
    
});

});