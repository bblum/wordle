// use this to get react canvas into the clipboard
// requires chrome not firefox
// paste it into your js console before running the solver
// really should do this with an event listener but this is easy and plenty fast enough
function hax() { navigator.clipboard.writeText(document.children[0].innerHTML); setTimeout(hax, 10); } setTimeout(hax, 1000);
