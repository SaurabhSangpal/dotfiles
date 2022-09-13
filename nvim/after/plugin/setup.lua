local lsp = require 'lspconfig'

lsp.gopls.setup{}
lsp.clangd.setup{}
lsp.rls.setup {
    settings = {
        rust = {
            unstable_features = true,
            build_on_save = false,
            all_features = true,
        },
    },
}
lsp.omnisharp.setup{}
lsp.gdscript.setup{}
