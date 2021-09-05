library(ggplot2)
library(ggtree)
library(phytools)
library(ggtext)

## ~/Downloads/RAxML_bipartitions.out30bs
tree<-read.tree("~/Desktop/data/concat_bt100.nwk")
tree<-phytools::reroot(tree,findMRCA(tree,c("Aspacu1","Aspell1")),position=0.5*tree$edge.length[which(tree$edge[,2]==findMRCA(tree,c("Aspacu1","Aspell1")))])

##nucl skip this
tree$tip.label <-replace(tree$tip.label,tree$tip.label == "Asptu1","Asptub1")
for (i in tree$tip.label){
if (startsWith(i,"Aspergillus")){
    id <- sapply(strsplit(i,"_IFM"),identity)[2]
    tree$tip.label <- replace(tree$tip.label,tree$tip.label == i,id)
}
}

##change tip labels
ns=c("61612","57143","55763","56815","54640","63604","63309")
D <- c("A. neoniger-CBS 115656","A. tubingensis-CBS 134.48","A. luchuensis-CBS 106.47","A. piperis-CBS 112811","A. eucalypticola-CBS 122712",
        "A. vadensis-CBS 113365","A. costaricaensis-CBS 115574","A. welwitschiae-CBS 557.65","A. niger-CBS 513.88","A. lacticoffeatus-CBS 101883",
        "A. brasiliensis-CBS 101740","A. japonicus-CBS 114.51","A. aculeatinus-CBS 121060","A. ellipticus-CBS 482.65","A. carbonarius-CBS 111.26")

names(D) <- c("Aspneo1","Asptub1","Aspfo1","Asppip1","Aspeuc1",
"Aspvad1","Aspcos1","Aspwel1","Aspni_DSM_1","Asplac1",
"Aspbr1","Aspjap1","Aspacu1","Aspell1","Aspca3")

labels <- tree$tip.label

for (i in labels) {
    if (startsWith(i,"A")){
        splits <- sapply(strsplit(D[[i]],"-"),identity)
        out <- paste("paste(italic(\'",splits[1],"\'),\'",splits[2],"\')")
        labels <- replace(labels,labels == i,out)
    } else {
        if (i %in% ns) {
            out <- paste("paste(bold(\'IFM\'),","bold(\'",i,"\'))")
        } else {
            out <- paste("paste(\'IFM\',","\'",i,"\')")
        }
        labels <- replace(labels,labels == i,out)
    }
}

tree$tip.label<-labels
## check tip
ggtree(tree) +geom_text(aes(label=label))

##nucl
p <-
ggtree(tree,branch.length="none")+ geom_treescale(x=0,y=0,fontsize=3,offset=0.2)+
geom_text2(aes(label=label,subset=!isTip & as.numeric(label) > 70),vjust=-0.5,hjust=3,size=2.5)+
xlim(0,10) +
geom_tiplab(parse=T,family="Times New Roman")
p


"""
+
geom_cladelabel(parse=T,node=34,offset=0.15,label=paste("paste(italic(\'A. tubingensis \'),","\'clade\')"))+
geom_cladelabel(parse=T,node=47,offset=0.15,label=paste("paste(italic(\'A. niger \'),","\'clade\')"))+
geom_cladelabel(node=33,offset=0.3,label="Biseriates",barsize=2)+
geom_strip('Aspca3','Aspell1',offset=0.3,label="Uniseriates",barsize=2)
"""
## amino acid
p <-
ggtree(tree)+ geom_treescale(x=0,y=0,fontsize=3,offset=0.2)+
xlim(0,0.3)+ theme(legend.position="none")

p <- p + geom_tiplab(parse=T,align=T,family="Times New Roman")+
geom_hilight(node=52,fill="darkgreen",alpha=0.4,extend=0.2)+
geom_hilight(node=39,fill="steelblue",alpha=0.4,extend=0.2)
p

#ggtree(tree,branch.length="none")+geom_text(aes(label=node),alpha=0.5)+geom_tiplab(parse=T)
#ggtree(tree)+geom_text(aes(label=node),alpha=0.5)+geom_tiplab(parse=T)
flip(p,46,34)


