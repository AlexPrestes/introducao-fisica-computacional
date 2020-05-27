using CSV
using DataFrames
using Plots

function le_arquivo(nomearquivo, delim=" ", header=false)
    dados = CSV.read(nomearquivo, delim=delim, header=header)
    return dados
end

function plota_dados(x, y, label, axislabel = [], axisscale = [:none, :none])
    plot(x,y,
         label=label,
         w=[3 3 3],
         xaxis=axisscale[1],
         yaxis=axisscale[2],
         xlabel=axislabel[1],
         ylabel=axislabel[2],
         legendfontsize=10,
         legend=:best,
         fontsize=12,
         tickfontsize=12
        )
end
