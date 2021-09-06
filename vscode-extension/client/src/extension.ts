/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';
import * as vscode from 'vscode';
import { workspace, ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

function getServerPath(): string {
    const serverPath = vscode.workspace.getConfiguration('fea-lsp').get('server.path');
    if (typeof serverPath === 'string') {
        if (serverPath.startsWith("~/")) {
            return os.homedir() + serverPath.slice("~".length);
        }
        return serverPath;
    } else {
        return "I_NEED_TO_BE_SET";
    }
}

export function activate(context: ExtensionContext) {
    const serverPath = getServerPath();
    if (!fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(
            `Path '${serverPath}' does not exist. fea-lsp.server.path must point to path of fea-lsp binary.`,
        );
        return;
    }

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'FEA' }],
        // synchronize: {
        // 	// Notify the server about file changes to '.clientrc files contained in the workspace
        // 	fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        // }
    };

    // const command = serverPath;

    // Create the language client and start the client.
    client = new LanguageClient(
        'fea-lsp',
        'fea-lsp',
        { command: serverPath, options: { env: { "RUST_BACKTRACE": 1 } } },
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
    console.log("started server!?");

}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
