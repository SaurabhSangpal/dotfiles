vim.g.mapleader = ","

-- Helper functions

function map(mode, shortcut, command, other)
    vim.api.nvim_set_keymap(mode, shortcut, command, other)
end

function nmap(shortcut, command)
    map("n", shortcut, command, { noremap = true} )
end

function imap(shortcut, command)
    map("i", shortcut, command, { noremap = true } )
end

-- Helper functions end

imap("<leader><leader>", "<Esc>")
nmap("<C-e>", "5<C-e>")
nmap("<C-y>", "5<C-y>")
nmap("<leader>f", ":Ex<Cr>") -- Explorer

-- Telescope
nmap("<leader>ff", "<cmd>Telescope find_files<cr>")
nmap("<leader>fg", "<cmd>Telescope live_grep<cr>")
nmap("<leader>fb", "<cmd>Telescope buffers<cr>")
nmap("<leader>fh", "<cmd>Telescope help_tags<cr>")

-- LSP
--local opts = { noremap = true, silent = true }
--map("n", "<space>e", vim.diagnostic.open_float, opts)
--map("n", "[d", vim.diagnostic.goto_prev, opts)
--map("n", "]d", vim.diagnostic.goto_next, opts)
--map("n", "<space>q", vim.diagnostic.setloclist, opts)
--
--local on_attach = function(client, bufnr)
--    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
--
--    local bufopts = { noremap = true, silent = true, buffer = bufnr }
--    map("n", "gD", vim.lsp.buf.declaration, bufopts)
--    map("n", "gd", vim.lsp.buf.definition, bufopts)
--    map("n", "K", vim.lsp.buf.hover, bufopts)
--    map("n", "gi", vim.lsp.buf.implementation, bufopts)
--    map("n", "<C-k", vim.lsp.buf.signature_help, bufopts)
--    map("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, bufopts)
--    map("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
--    map("n", "<leader>wl", function()
--        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
--    end, bufopts)
--    map("n", "<leader>D", vim.lsp.buf.type_definition, bufopts)
--    map("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
--    map("n", "<leader>ca", vim.lsp.buf.code_action, bufopts)
--    map("n", "gr", vim.lsp.buf.references, bufopts)
--    map("n", "<leader>e", vim.lsp.buf.formatting, bufopts)
--end
--
--local lsp_flags = {
--    debounce_text_changes = 150,
--}
