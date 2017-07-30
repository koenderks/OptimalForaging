$(document).keyup(function(event) {
    if ($("#word2").is(":focus") && (event.keyCode == 13)) {
        $("#submit2").click();
        $("#word2").val("");
    }
});

$(document).ready(function() {

$("#submit2").on("click", function() {
    
    $("#word2").val("");
    
});

});