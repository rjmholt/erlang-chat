var net      = require('net');

// Incoming message types
const S_MESSAGE    = 'message';
const NEWIDENTITY  = 'newidentity';
const ROOMCHANGE   = 'roomchange';
const ROOMLIST     = 'roomlist';
const ROOMCONTENTS = 'roomcontents';

// Outgoing message types
const C_MESSAGE      = 'message';
const JOIN           = 'join';
const QUIT           = 'quit';
const IDENTITYCHANGE = 'identitychange';
const WHO            = 'who';
const LIST           = 'list';
const CREATEROOM     = 'createroom';
const DELETE         = 'delete';
const KICK           = 'kick';

// Client states
const AWAITING_NAME = 1;
const AWAITING_ROOM = 2;
const READY         = 3;

var clientState = { readiness:  AWAITING_NAME,
                    name:       '',
                    room:       '',
                    prompt: 
                        function () {
                            if (this.name !== '' && this.room !== '') {
                                process.stdout.write('[' + this.room + '] ' +
                                    this.name + '> ');
                            }
                        },
                    receiveMessage: (m) => initialHandshake(m, this.readiness),
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
        console.log('Message received:', msg);
        clientState.receiveMessage(msg);
        clientState.receiveMessage = getNextFunction(clientState.readiness);
    }
}

function getNextFunction(state) {
    switch (state) {
    case READY:
        return (m) => processMessage(m);

    case AWAITING_NAME:
        return (m) => initialHandshake(m, AWAITING_NAME);

    case AWAITING_ROOM:
        return (m) => initialHandshake(m, AWAITING_ROOM);
    }

    // If nothing is going right, just help the user to get themselves set up
    return (m) => initialHandshake(m, READY);
}

function initialHandshake(msg, state) {
    if (typeof msg !== 'object') {
        console.log('Did not receive and object:', msg);
        return;
    }

    switch (msg.type) {
    case NEWIDENTITY:
        if (msg.former === clientState.name && msg.former === '') {
            clientState.name = msg.identity;
            clientState.readiness = AWAITING_ROOM;
        }
        break;

    case ROOMCHANGE:
        if (msg.former === clientState.room && msg.former === '') {
            clientState.room = msg.roomid;
            clientState.readiness = READY;
            clientState.prompt();
        }
        break;
    }
    return;
}

function processMessage(msg) {
    if (typeof msg !== 'object') {
        console.log('Message is not an object:', msg);
        return;
    }

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

    clientState.prompt();
}

function processUserInput(input) {
    // No hash means input entered was a message
    if (input[0] !== '#') {
        return { type: C_MESSAGE, message: input.slice(0, input.length-1) };
    }
    // Otherwise it's a command
    else {
        var tokens = input.slice(1).split(/\s+/);

        switch (tokens[0]) {
        case JOIN:
            if (tokens.length !== 2) {
                clientState.prompt();
                return;
            }
            return { type: JOIN, roomid: tokens[1] };

        case IDENTITYCHANGE:
            if (tokens.length !== 2) {
                clientState.prompt();
                return;
            }
            return { type: IDENTITYCHANGE, identity: tokens[1] };

        case QUIT:
            return { type: QUIT };

        case WHO:
            if (tokens.length !== 2) {
                clientState.prompt();
                return;
            }
            return { type: WHO, roomid: tokens[1] };

        case LIST:
            return { type: LIST };

        case CREATEROOM:
            if (tokens.length !== 2) {
                clientState.prompt();
                return;
            }
            return { type: CREATEROOM, roomid: tokens[1] };

        case DELETE:
            if (tokens.length !== 2) {
                return;
            }
            return { type: DELETE, roomid: tokens[1] };

        case KICK:
            if (tokens.length !== 4) {
                clientState.prompt();
                return;
            }
            return { type: KICK, roomid: tokens[1],
                     time: parseInt(tokens[2], 10), identity: tokens[3]};

        // First part doesn't recognise a supported command,
        // send it as an ordinary message instead
        default:
            return { type: C_MESSAGE, message: input };
        }
    }
}

function doInputMessage(inBuf) {
    var message = processUserInput(inBuf.toString('utf8'));
    if (typeof message === 'object') {
        console.log(message);
        client.write(JSON.stringify(message), 'utf8');
    }
}
