-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use 'gruvbox-community/gruvbox'
  use 'folke/tokyonight.nvim'
  use { 'catppuccin/nvim', as = 'catppuccin' }
  use 'sheerun/vim-polyglot'

  use 'nvim-lua/popup.nvim'
  use {
    'nvim-lualine/lualine.nvim',
    requires = { { 'kyazdani42/nvim-web-devicons', opt = true } }
  }

  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/nvim-cmp'
  use 'onsails/lspkind-nvim'
  use 'nvim-lua/lsp_extensions.nvim'
  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'
  use { 'tzachar/cmp-tabnine', run='./install.sh' }

  use 'mfussenegger/nvim-dap'
  use 'rcarriga/nvim-dap-ui'
  use 'theHamsta/nvim-dap-virtual-text'

  use ('nvim-treesitter/nvim-treesitter', {
      run = ":TSUpdate"
  })
  use {
      'nvim-telescope/telescope.nvim', tag = '0.1.x',
      requires = { {'nvim-lua/plenary.nvim'} }
  }
end)
