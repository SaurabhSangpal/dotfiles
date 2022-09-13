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
