var importObject = { imports: { imported_func: arg => console.log(arg) } };
fetch('memory.wasm').then(response =>
    response.arrayBuffer()
).then(bytes =>
    WebAssembly.instantiate(bytes, importObject)
).then(results => {
    var button = document.getElementById('run');
    button.addEventListener('click', function() {
        console.log(results.instance.exports.main());
    })
});
