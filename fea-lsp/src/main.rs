use lspower::{jsonrpc::Result, lsp::*, Client, LanguageServer, LspService, Server};
use serde_json::Value;

mod document;

#[derive(Debug)]
struct Backend {
    client: Client,
    //text: Mutex<String>,
    document: document::Document,
}

#[lspower::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: Some(false),
                            },
                            legend: SemanticTokensLegend {
                                token_types: document::STYLES.into(),
                                token_modifiers: Vec::new(),
                            },
                        },
                    ),
                ),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Incremental,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    ..Default::default()
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    ..Default::default()
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::Info, "funitialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::Info, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::Info, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::Info, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::Info, "command executed!")
            .await;

        match self
            .client
            .apply_edit(WorkspaceEdit::default(), Default::default())
            .await
        {
            Ok(res) if res.applied => self.client.log_message(MessageType::Info, "applied").await,
            Ok(_) => self.client.log_message(MessageType::Info, "rejected").await,
            Err(err) => self.client.log_message(MessageType::Error, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, doc: DidOpenTextDocumentParams) {
        self.document.set_text(doc.text_document.text);
        //*self.text.lock().unwrap() = doc.text_document.text;
        self.client
            .log_message(MessageType::Info, "file opened!")
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.pop() {
            self.document.replace_range(change.range, change.text)
        }
        self.client
            .log_message(MessageType::Info, "file changed!")
            .await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file closed!")
            .await;
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn semantic_tokens_full(
        &self,
        _params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client
            .log_message(MessageType::Info, "semantic tokens full!?")
            .await;

        //let text: String = self.text.lock().unwrap().clone();
        let tokens = self.document.semantic_tokens();
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();
    eprintln!("what is happen");

    let (service, messages) = LspService::new(|client| Backend {
        client,
        document: Default::default(),
    });
    Server::new(tokio::io::stdin(), tokio::io::stdout())
        .interleave(messages)
        .serve(service)
        .await;

    Ok(())
}
