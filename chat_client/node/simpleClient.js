var net      = require('net');

var clientState = { name: '',
                    room: '',
                    prompt: function () {
                        if (this.name !== '' && this.room !== '') {
                            process.stdout.write('[' + this.room + '] ' +
                                this.name + '> ');
                        }
                    }
                  };

var client = new net.Socket();
client.setEncoding('utf8');

client.on('data', receiveData);

client.connect(7777, 'localhost', function () {
    process.stdin.addListener('data', doInputMessage);
});

// --------------- FUNCTIONS --------------------------------------------------

function receiveData(data) {
    var objStrings = Buffer.from(data, 'utf8').toString('utf8').split('\n');
    objStrings.pop();

    for (var objStr of objStrings) {
        var msg = JSON.parse(objStr);
        processMessage(msg);
        clientState.prompt();
    }
}

function processMessage(msg) {
    console.log('Message Received:', msg);

    switch (msg.type) {
        case 'message':
            // Display ordinary chat messages
            console.log(msg.identity + ': ' + msg.message);
            break;

        case 'newidentity':
            if (msg.former === msg.identity) {
                // Name change has been rejected
                break;
            }
            // Test if the message is about me
            if (msg.former === clientState.name) {
                // Change my name if it is
                clientState.name = msg.identity;
                // If I used to have no name, then I'm new
                if (msg.former === '') {
                    console.log('You have joined chat as ' + msg.identity);
                }
                // Otherwise I changed my name
                else {
                    console.log('You have changed name from ' + msg.former +
                            ' to ' + msg.identity);
                }
            }
            else {
                // Somebody else used to have no name => they are new
                if (msg.former === '') {
                    console.log(msg.identity + ' has joined');
                }
                // Otherwise they've changed their name
                else {
                    console.log(msg.former + ' is now ' + msg.identity);
                }
            }
            break;

        case 'roomchange':
            if (msg.former === msg.roomid) {
                // Room change has been rejected
                break;
            }
            // Test if the message is about me
            if (msg.identity === clientState.name) {
                clientState.room = msg.roomid;
                if (msg.former === '') {
                    console.log('Now connected. In room ' + msg.roomid);
                }
                else if (msg.roomid === '') {
                    process.exit(0);
                }
                else {
                    console.log('You moved from ' + msg.former +
                            ' to ' + msg.roomid);
                }
            }
            else {
                if (msg.former === '') {
                    console.log(msg.identity + ' has joined the server');
                }
                else if (msg.roomid === '') {
                    console.log(msg.identity + ' has left the server');
                }
                else {
                    console.log(msg.identity + ' has moved from ' +
                            msg.former + ' to ' + msg.roomid);
                }
            }
            break;

        default:
            console.log('Unknown message:', msg);
    }
}

function processUserInput(input) {
    // No hash means input entered was a message
    if (input[0] !== '#') {
        return { type: 'message', message: input.slice(0, input.length-1) };
    }
    // Otherwise it's a command
    else {
        var tokens = input.slice(1).split(/\s+/);

        switch (tokens[0]) {
        case 'join':
            if (tokens.length !== 2) {
                return;
            }
            return { type: tokens[0], roomid: tokens[1] };

        case 'identitychange':
            if (tokens.length !== 2) {
                return;
            }
            return { type: tokens[0], identity: tokens[1] };

        case 'quit':
            return { type: tokens[0] };

        case 'who':
            if (tokens.length !== 2) {
                return;
            }
            return { type: tokens[0], roomid: tokens[1] };

        case 'list':
            return { type: tokens[0] };

        case 'createroom':
            if (tokens.length !== 2) {
                return;
            }
            return { type: tokens[0], roomid: tokens[1] };

        case 'delete':
            if (tokens.length !== 2) {
                return;
            }
            return { type: tokens[0], roomid: tokens[1] };

        case 'kick':
            if (tokens.length !== 4) {
                return;
            }
            return { type: tokens[0], roomid: tokens[1],
                     time: parseInt(tokens[2], 10), identity: tokens[3]};

        // First part doesn't recognise a supported command,
        // send it as an ordinary message instead
        default:
            return { type: 'message', message: input };
        }
    }
}

function doInputMessage(inBuf) {
    var message = processUserInput(inBuf.toString('utf8'));
    console.log(message);
    client.write(JSON.stringify(message), 'utf8');
}
