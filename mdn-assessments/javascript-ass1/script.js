var customName = document.getElementById('customname');
var randomize = document.querySelector('.randomize');
var story = document.querySelector('.story');

function randomValueFromArray(array) {
    return array[Math.floor(Math.random() * array.length)];
}
var storyText = 'It was 94 farenheit outside, ' +
    'so :insertx: went for a walk. When they got to :inserty: , ' +
    'they stared in horror for a few moments, then :insertz:. ' +
    'Bob saw the whole thing, but he was not surprised — ' +
    ':insertx: weighs 300 pounds, and it was a hot day.';
var insertX = ['Willy the Goblin', 'Big Daddy', 'Father Christmas'];
var insertY = ['the soup kitchen', 'Disneyland', 'the White House'];
var insertZ = [
    'spontaneously combusted',
    'melted into a puddle on the sidewalk',
    'turned into a slug and crawled away'
];
randomize.addEventListener('click', result);

function result() {
    var trimmedName = customName.value.trim();
    if (trimmedName === '') {
        return;
    }
    var itemX = randomValueFromArray(insertX);
    var itemY = randomValueFromArray(insertY);
    var itemZ = randomValueFromArray(insertZ);
    var theStory = storyText.replace(new RegExp(/:insertx:/, 'g'), itemX);
    theStory = theStory.replace(':inserty:', itemY);
    theStory = theStory.replace(':insertz:', itemZ);
    theStory = theStory.replace('Bob', trimmedName);
    if (document.getElementById("uk").checked) {
        var weight = Math.round(300);
        var temperature = Math.round(94);
        var celcius = Math.round((temperature - 32) * 5 / 9);
        var stone = Math.round(weight / 14);
        theStory = theStory.replace(weight, stone);
        theStory = theStory.replace(temperature, celcius);
        theStory = theStory.replace('pounds', 'stones');
        theStory = theStory.replace(' farenheit', ' centigrade');
    }
    story.textContent = theStory;
    story.style.visibility = 'visible';
}

