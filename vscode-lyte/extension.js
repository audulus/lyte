const { workspace, window } = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;

function activate(context) {
    const config = workspace.getConfiguration("lyte");
    const command = config.get("lspPath", "lyte-lsp");

    const serverOptions = {
        run: { command, transport: TransportKind.stdio },
        debug: { command, transport: TransportKind.stdio },
    };

    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "lyte" }],
    };

    client = new LanguageClient(
        "lyte-lsp",
        "Lyte Language Server",
        serverOptions,
        clientOptions
    );

    client.start();
}

function deactivate() {
    if (client) {
        return client.stop();
    }
}

module.exports = { activate, deactivate };
