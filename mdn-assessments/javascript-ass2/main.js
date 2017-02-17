var displayedImage = document.querySelector('.displayed-img');
var thumbBar = document.querySelector('.thumb-bar');
btn = document.querySelector('button');
var overlay = document.querySelector('.overlay');
/* Looping through images */
function getImagePathByIndex(index) {
    return 'images/pic' + index + '.jpg';
}
for (var i = 1; i <= 5; ++i) {
    var newImage = document.createElement('img');
    var imagePath = getImagePathByIndex(i);
    newImage.setAttribute('src', imagePath);
    thumbBar.appendChild(newImage);
    newImage.onclick = function(e) {
        displayedImage.setAttribute('src', e.target.getAttribute('src'));
    };
}
/* Wiring up the Darken/Lighten button */
btn.onclick = function() {
    if (btn.textContent == 'Darken') {
        btn.textContent = 'Lighten';
        overlay.style.backgroundColor = 'rgba(0, 0, 0, 0.5)';
    } else {
        btn.textContent = 'Darken';
        overlay.style.backgroundColor = 'rgba(0, 0, 0, 0)';
    }
};

