-- vim.cmd("colorscheme tokyonight")
-- vim.g.catppuccin_flavour = "mocha"
-- require("catppuccin").setup({
--     transparent_background = true,
-- })
-- vim.cmd [[colorscheme catppuccin]]
require("tokyonight").setup({
  -- use the night style
  style = "storm",
  transparent = false,
  -- disable italic for functions
  styles = {
    functions = {}
  },
  sidebars = { "qf", "vista_kind", "terminal", "packer" },
  -- Change the "hint" color to the "orange" color, and make the "error" color bright red
  on_colors = function(colors)
    colors.hint = colors.orange
    colors.error = "#ff0000"
  end
})

vim.cmd [[colorscheme tokyonight]]

require('lualine').setup {
  options = {
    theme = 'tokyonight'
  }
}
